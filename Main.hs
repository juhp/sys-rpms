module Main (main) where

import Control.Monad
import Data.Char
import Data.List
import Data.Time.Clock
import Data.Time.Format
import SimpleCmd
import SimpleCmdArgs
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.Directory
import System.FilePath

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
    ]

-- FIXME --versions ?
saveCmd :: IO ()
saveCmd = do
  basefile <- getCacheFile Nothing
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

-- FIXME --force/--delete
getCacheFile :: Maybe String -> IO FilePath
getCacheFile mid = do
  ident <- maybe getSystemId return mid
  dir <- getUserCacheDir "sys-rpms"
  return $ dir </> ident

getSystemId :: IO String
getSystemId = do
  cgroup <- takeBaseName <$> readFile "/proc/self/cgroup"
  take 12 <$> case span (isLetter) cgroup of
                ("libpod", cid) -> return $ tail cid
                ("docker", cid) -> return $ tail cid
                _ -> readFile "/etc/machine-id"

latestCacheFile :: FilePath -> IO FilePath
latestCacheFile path = do
  let (dir,base) = splitFileName path
  files <- sort . filter (base `isPrefixOf`) <$> listDirectory dir
  return $ removePrefix base $ last files

diffCmd :: Maybe String -> IO ()
diffCmd mid = do
  basefile <- getCacheFile mid
  latest <- latestCacheFile basefile
  pipe3_ ("rpm",rpmqaArgs) ("sort",[]) ("diff",["-u0", basefile ++ latest, "-"])

rpmqaArgs :: [String]
rpmqaArgs = ["-qa", "--qf", "%{name}\n"]

listCmd :: IO ()
listCmd = do
  dir <- getUserCacheDir "sys-rpms"
  files <- filter (not . ("." `isInfixOf`)) <$> listDirectory dir
  machineid <- take 12 <$> readFile "/etc/machine-id"
  ident <- getSystemId
  forM_ files $ \ file -> do
    putStr file
    if file == machineid
      then putStr $ " hostsystem"
      -- FIXME use "--format {{.NAMES}}" when it is unbroken
      else do
      -- FIXME support docker too, anything else?
      podman <- findExecutable "podman"
      case podman of
        Nothing -> putStr $ " container"
        Just _ -> do
          name <- last . words . last <$> cmdLines "podman" ["ps", "--filter", "id=" ++ file]
          putStr $ " " ++ name
    putStrLn $ if file == ident then " [local]" else ""
