------------------------------------------------------------------------
-- |
-- Module      : Haskal.Config
-- License     : GPL
--
------------------------------------------------------------------------
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
-- 02110-1301, USA.

module Haskal.Config
  ( configuration
  ) where

import System.Console.GetOpt
import System.Plugins
import Haskal.Util
import Control.Exception as Ex
import System.IO
import System.Directory
import System.Environment
import System.Exit
import Haskal.ConfigAPI
import Haskal.Path ( expand, (</>) )

import Paths_haskal ( getDataFileName )


options :: [OptDescr (Config -> IO Config)]
options = 
  [ Option "v" ["verbose"] (NoArg setVerbose) "verbose mode"
  , Option "V" ["version"] (NoArg printVersion) "print version"
  , Option "c" ["command"] (ReqArg execCmd "COMMAND") "execute command"
  , Option "d" ["directory"] (ReqArg setDir "DIR") "configuration directory"
  , Option "h" ["help"] (NoArg printHelp) "print help and exit"
  ] where
      setVerbose c   = return $ c { verbose = True }
      setDir     s c = return $ c { dir = s }
      printVersion c = putStrLn "haskal 0.1" >> return c
      execCmd _ _    = putStrLn "Not implemented" >> exitFailure
      printHelp _    = do putStrLn (usageInfo "USAGE:" options)
                          exitWith ExitSuccess

loadOrReload
  :: String            -- ^ The haskell source file.
  -> String            -- ^ Stub file to make With.
  -> [String]          -- ^ Arguments to make.
  -> [String]          -- ^ Includes to load.
  -> String            -- ^ The symbol to load.
  -> Maybe (Module, a) -- ^ Previously loaded module.
  -> IO (Module, a)
loadOrReload file stub args incls sym mInfo = do
  ms <- makeWith file stub args
  case (ms, mInfo) of
    (MakeFailure es, _                   ) -> mapM_ putStrLn es >> exitFailure
    (MakeSuccess ReComp _obj, Just (m, _)) -> reload m sym >>= loadStat
    (MakeSuccess _ obj, Nothing          ) -> load_ obj incls sym >>= loadStat
    (_, Just (m, v)                      ) -> return (m, v)
  where
    loadStat (LoadFailure es)  = mapM_ putStrLn es >> exitFailure
    loadStat (LoadSuccess m v) = return (m, v)

configuration :: Maybe (Module, Config) -> IO (Module, Config)
configuration mCfg = do
  (configFile, configStub) <- createFiles
  (m, cfg) <- loadOrReload configFile configStub ["-package haskal"]
                           [] "config" mCfg
  cfg' <- expand (dir cfg) >>= \dir' -> return $ cfg { dir = dir' }
  (opts, _nonOpts, errs) <- liftM (getOpt Permute options) getArgs
  case errs of
    [] -> liftM ((,) m) $ foldl' (>>=) (return cfg') opts
    _  -> do
      mapM_ print errs
      print (usageInfo "USAGE:" options)
      exitFailure

createFiles :: IO (String, String)
createFiles = do
  cfgDir  <- expand $ dir defaultConfig
  cfgStub <- getDataFileName ("src" </> "config_stub")
  let cfgFile = cfgDir </> "Config.hs"
  Ex.catch (createDirectory cfgDir) (\(_::SomeException) -> return ())
  openFile cfgFile AppendMode >>= hClose
  return (cfgFile, cfgStub)
