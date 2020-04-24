module Main (main) where

import Data.Char
import Data.List
import Data.Time.Format
import Data.Time.LocalTime
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
      pure diffCmd
    -- , Subcommand "list" "compare current installed rpms with saved list" $
    --   pure diffCmd
    ]

saveCmd :: IO ()
saveCmd = do
  basefile <- getCacheFile
  exists <- doesFileExist basefile
  rpms <- sort <$> cmdLines "rpm" rpmqaArgs
  mstore <-
    if exists then do
      origrpms <- lines <$> readFile basefile
      if rpms == origrpms then return Nothing
        else do
        zt <- getZonedTime
        let timestamp = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) zt
        return $ Just $ basefile <.> timestamp
    else return $ Just basefile
  case mstore of
    Nothing -> putStrLn "no change"
    Just store -> do
      writeFile store $ unlines rpms
      putStrLn store

-- FIXME --force/--delete
getCacheFile :: IO FilePath
getCacheFile = do
  cgroup <- takeBaseName <$> readFile "/proc/self/cgroup"
  ident <- take 12 <$> case span (isLetter) cgroup of
            ("libpod", cid) -> return $ tail cid
            ("docker", cid) -> return $ tail cid
            _ -> readFile "/etc/machine-id"
  dir <- getUserCacheDir "sys-rpms"
  return $ dir </> ident

latestCacheFile :: FilePath -> IO FilePath
latestCacheFile path = do
  let (dir,base) = splitFileName path
  files <- sort . filter (base `isPrefixOf`) <$> listDirectory dir
  return $ removePrefix base $ last files

diffCmd :: IO ()
diffCmd = do
  basefile <- getCacheFile
  latest <- latestCacheFile basefile
  pipe3_ ("rpm",rpmqaArgs) ("sort",[]) ("diff",["-u0", basefile ++ latest, "-"])

rpmqaArgs :: [String]
rpmqaArgs = ["-qa", "--qf", "%{name}\n"]
