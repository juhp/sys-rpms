{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Monad
import Data.List
import Data.List.Split
import Data.Time.Clock
import Data.Time.Format
import SimpleCmd
import SimpleCmdArgs
import System.Environment
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.Directory
import System.FilePath
import System.Posix.Process

import Paths_sys_rpms (version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "Record installed rpm packages"
    "This tool stores package lists to know what packages have been added" $
    subcommands
    [ Subcommand "save" "save rpm list for this system/container" $
      pure saveCmd
    , Subcommand "diff" "compare current installed rpms with saved list" $
      diffCmd <$> optional (strArg "ID")
    , Subcommand "list" "list of rpm systems saved" $
      pure listCmd
    , Subcommand "show" "show saved package list" $
      showCmd <$> optional (strArg "ID")
    , Subcommand "current" "output current system rpms" $
      pure currentCmd
    ]

-- FIXME --versions ?
saveCmd :: IO ()
saveCmd = do
  basefile <- getBaseFile
  exists <- doesFileExist basefile
  rpms <- sort <$> cmdLines "rpm" rpmqaArgs
  mstore <-
    if exists then do
      origrpms <- lines <$> readFile basefile
      if rpms == origrpms then return Nothing
        else do
        zt <- getCurrentTime
        let timestamp = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" zt
        return $ Just $ basefile <.> timestamp
    else return $ Just basefile
  case mstore of
    Nothing -> putStrLn "no change"
    Just store -> do
      writeFile store $ unlines rpms
      putStrLn store

getBaseFile :: IO FilePath
getBaseFile = do
  dir <- getUserCacheDir "sys-rpms"
  ident <- getSystemId
  return $ dir </> ident

-- FIXME? --force/--delete
getCacheFile :: IO FilePath
getCacheFile = do
  base <- getBaseFile
  ts <- latestCacheTimeStamp base
  return $ base ++ ts

getSystemId :: IO String
getSystemId = do
  name <- getSystemName
  container <- doesFileExist "/run/.containerenv"
  ((name ++ "-") ++) . take 12 <$> if container
    then getParentProcessID >>= getContainerId . show
    else readFile "/etc/machine-id"
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
        let mppid = find ("PPid:" `isInfixOf`) status
        case mppid of
          Nothing -> error' "could not determine containerid"
          Just ppid -> getContainerId $ removePrefix "PPid:\t" ppid

latestCacheTimeStamp :: FilePath -> IO FilePath
latestCacheTimeStamp path = do
  let (dir,base) = splitFileName path
  files <- sort . filter (base `isPrefixOf`) <$> listDirectory dir
  return $ if null files then error' $ base ++ " not found in " ++ dir
    else removePrefix base $ last files

diffCmd :: Maybe String -> IO ()
diffCmd msysid = do
  sysid <- case msysid of
    Nothing -> getCacheFile
    Just sid -> do
      dir <- getUserCacheDir "sys-rpms"
      return $ dir </> sid
  localrpms <- sort <$> cmdLines "rpm" rpmqaArgs
  diff <- cmdIgnoreErr "diff" ["-u0", sysid, "-"] $ unlines localrpms
  mapM_ putStrLn $ filter (not . ("@@ " `isPrefixOf`)) $ lines diff

rpmqaArgs :: [String]
rpmqaArgs = ["-qa", "--qf", "%{name}\n"]

listCmd :: IO ()
listCmd = do
  dir <- getUserCacheDir "sys-rpms"
  systems <- filter (not . ("." `isInfixOf`)) <$> listDirectory dir
  machineid <- take 12 <$> readFile "/etc/machine-id"
  ident <- getSystemId
  forM_ systems $ \ sys -> do
    timestamp <- latestCacheTimeStamp (dir </> sys)
    putStr $ sys ++ timestamp
    when (machineid `isSuffixOf` sys) $ putStr " [host]"
    putStrLn $ if sys == ident then " [local]" else ""

showCmd :: Maybe String -> IO ()
showCmd msysid = do
  sysid <- case msysid of
    Nothing -> getCacheFile
    Just sid -> do
      dir <- getUserCacheDir "sys-rpms"
      ifM (doesFileExist (dir </> sid)) (return $ dir </> sid) $ do
        timestamp <- latestCacheTimeStamp (dir </> sid)
        return $ dir </>  sid ++ timestamp
  readFile sysid >>= putStr

currentCmd :: IO ()
currentCmd =
  cmdLines "rpm" rpmqaArgs >>= mapM_ putStrLn . sort

#if (defined(MIN_VERSION_simple_cmd) && MIN_VERSION_simple_cmd(0,1,4))
#else
error' :: String -> a
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,9,0))
error' = errorWithoutStackTrace
#else
error' = error
#endif
#endif
