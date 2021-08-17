{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Monad.Extra
import qualified Data.List as L
import Data.List.Extra
import Data.Time.Clock
import Data.Time.Format
import SimpleCmd
#if MIN_VERSION_simple_cmd(0,2,1)
  hiding (ifM)
#endif
import SimpleCmdArgs
import System.Environment
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.Directory
import System.FilePath
import System.Posix.Process

import Paths_sys_rpms (version)

main :: IO ()
main = do
  setDirectory
  simpleCmdArgs (Just version) "Record installed rpm packages"
    "This tool stores package lists to know what packages have been added" $
    subcommands
    [ Subcommand "save" "save rpm list for this system/container" $
      pure saveCmd
    , Subcommand "diff" "compare current installed rpms with saved list" $
      diffCmd <$> diffFilter <*> optional sysArg
    , Subcommand "list" "list of rpm systems saved" $
      pure listCmd
    , Subcommand "show" "show saved package list" $
      showCmd <$> optional sysArg
    , Subcommand "current" "output current system rpms" $
      pure currentCmd
    ]
  where
    setDirectory = do
      dir <- getUserCacheDir "sys-rpms"
      createDirectoryIfMissing True dir
      setCurrentDirectory dir

    diffFilter =
      flagWith' DiffAdded 'a' "added" "Show added packages" <|>
      flagWith DiffNormal DiffRemoved 'd' "removed" "Show removed packages"

    sysArg :: Parser System
    sysArg = readSystem <$> strArg "SYSID"

data DiffFilter = DiffNormal | DiffRemoved | DiffAdded

--                   name   id
data System = System String String
  deriving Eq

instance Show System where
  show (System n i) = n ++ "--" ++ i

readSystem :: String -> System
readSystem sys =
  case stripInfix "--" sys of
    Nothing -> error' $ "illegal system: " ++ sys
    Just (name,sid) -> System name sid

data SysRecord = SysRecord {_sysName :: String,
                            sysId ::String,
                            _sysTS :: String
                           }

instance Show SysRecord where
  show (SysRecord n i t) = show (System n i) <.> t

readSysRecord :: String -> SysRecord
readSysRecord sysrec =
  case stripInfix "--" sysrec of
    Nothing -> error' $ "illegal system record: " ++ sysrec
    Just (name,idts) ->
      case stripInfix "." idts of
        Nothing -> error' $ "missing timestamp: " ++ sysrec
        Just (sid,ts) -> SysRecord name sid ts

system :: SysRecord -> System
system (SysRecord n i _) = System n i

-- FIXME --versions ?
saveCmd :: IO ()
saveCmd = do
  sys <- getSystem
  mlatest <- maybeLatestRecord sys
  rpms <- unlines . L.sort <$> cmdLines "rpm" rpmqaArgs
  mstore <-
    case mlatest of
      Just sysrec -> do
        origrpms <- readFile $ show sysrec
        if rpms == origrpms
          then return Nothing
          else Just <$> newFilename sys
      Nothing -> Just <$> newFilename sys
  case mstore of
    Nothing -> putStrLn "no change"
    Just store -> do
      writeFile store rpms
      putStrLn store
  where
    newFilename base = do
        zt <- getCurrentTime
        let timestamp = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" zt
        return $ show base <.> timestamp

getRecord :: Maybe System -> IO SysRecord
getRecord msys =
  maybe getSystem return msys
  >>= latestRecord

getSystem :: IO System
getSystem = do
  name <- getSystemName
  container <- doesFileExist "/run/.containerenv"
  sysid <- take 12 <$>
    if container
    then getParentProcessID >>= getContainerId . show
    else readFile "/etc/machine-id"
  return $ System name sysid
  where
    getSystemName :: IO String
    getSystemName = do
      hostname <- readFile "/etc/hostname"
      case hostname of
        "toolbox" -> getEnv "DISTTAG"
        _ -> return $ takeWhile (/= '.') hostname

    getContainerId :: String -> IO String
    getContainerId pid = do
      let procpid = "/proc" </> pid
      comm <- readFile (procpid </> "comm")
      if comm == "conmon\n" then
        head . tail . dropWhile (/= "-c") . splitOn "\NUL" <$> readFile (procpid </> "cmdline")
        else do
        status <- lines <$> readFile (procpid </> "status")
        let mppid = L.find ("PPid:" `L.isInfixOf`) status
        case mppid of
          Nothing -> error' "could not determine containerid"
          Just ppid -> getContainerId $ removePrefix "PPid:\t" ppid

maybeLatestRecord :: System -> IO (Maybe SysRecord)
maybeLatestRecord sys = do
  sysrecs <- L.sort . filter isSystem <$> listDirectory "."
  return $ if null sysrecs
           then Nothing
           else Just $ readSysRecord $ last sysrecs
  where
    isSystem :: FilePath -> Bool
    isSystem file =
      show sys `L.isPrefixOf` file

latestRecord :: System -> IO SysRecord
latestRecord sys = do
  mlatest <- maybeLatestRecord sys
  case mlatest of
    Nothing -> error' $ show sys ++ " not found"
    Just file -> return file

diffCmd :: DiffFilter -> Maybe System -> IO ()
diffCmd dfilter msys = do
  sysrec <- getRecord msys
  localrpms <- L.sort <$> cmdLines "rpm" rpmqaArgs
  case dfilter of
    DiffNormal -> do
      diff <- cmdIgnoreErr "diff" ["-u0", show sysrec, "-"] $ unlines localrpms
      mapM_ putStrLn $ filter (not . ("@@ " `L.isPrefixOf`)) $ lines diff
    DiffRemoved -> do
      pkgs <- getSysRecordPkgs sysrec
      mapM_ putStrLn $ pkgs L.\\ localrpms
    DiffAdded -> do
      pkgs <- getSysRecordPkgs sysrec
      mapM_ putStrLn $ localrpms L.\\ pkgs

rpmqaArgs :: [String]
rpmqaArgs = ["-qa", "--qf", "%{name}\n"]

listCmd :: IO ()
listCmd = do
  systems <- map last . groupBy sameSystem . map readSysRecord . sort <$> listDirectory "."
  machineid <- take 12 <$> readFile "/etc/machine-id"
  ident <- getSystem
  forM_ systems $ \ sysrec -> do
    putStr $ show sysrec
    when (machineid == sysId sysrec) $ putStr " [host]"
    putStrLn $ if system sysrec == ident then " [local]" else ""
  where
    sameSystem :: SysRecord -> SysRecord -> Bool
    sameSystem (SysRecord n1 sid1 _) (SysRecord n2 sid2 _) =
      n1 == n2 && sid1 == sid2

getSysRecordPkgs :: SysRecord -> IO [String]
getSysRecordPkgs sysrec = do
  lines <$> readFile (show sysrec)

showCmd :: Maybe System -> IO ()
showCmd msys = do
  sysrec <- getRecord msys
  getSysRecordPkgs sysrec >>= mapM_ putStrLn

currentCmd :: IO ()
currentCmd =
  cmdLines "rpm" rpmqaArgs >>= mapM_ putStrLn . L.sort

#if !MIN_VERSION_simple_cmd(0,1,4)
error' :: String -> a
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,9,0))
error' = errorWithoutStackTrace
#else
error' = error
#endif
#endif
