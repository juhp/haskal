------------------------------------------------------------------------
-- |
-- Module      : Haskal.TypeOf
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

-- Adapted from HsPlugins

module Haskal.TypeOf
  ( typeOf_
  ) where

import System.Eval.Haskell()
import System.Eval.Utils
import System.Plugins.Make
import System.Plugins.Load
import System.Directory
import Data.Dynamic

typeOf_ :: String -> [String] -> [Import] -> IO String
typeOf_ src loadpath mods = do
    let src' = "{-# LINE 1 \"<typeOf>\" #-}\n" ++ src
    pwd                <- getCurrentDirectory
    tmpf               <- mkUniqueWith dynwrap src' mods
    status             <- make tmpf ["-i"++head loadpath, "-Onot","-fglasgow-exts","-package","plugins"]
    ty <- case status of
        MakeSuccess _ obj -> do
            m_v <- load_ obj ([pwd]++loadpath) "resource" :: IO (LoadStatus Dynamic)
            case m_v of
                LoadFailure _   -> return "<failure>"
                LoadSuccess _ v -> return $ (init . tail) $ show v

        MakeFailure err -> mapM_ putStrLn err >> return []
    makeCleaner tmpf
    return ty

dynwrap :: String -> String -> [Import] -> String
dynwrap expr nm mods 
    = unlines [ "module "++nm++ " (resource) where"
              ,  unlines . map ("import "++) $ mods 
              , "import Data.Dynamic" 
              , "resource = let { yhjulwwiefzojcbxybbruweejw = "
              , "{-# LINE 1 \"<eval>\" #-}\n" ++ expr ++ ";} in toDyn yhjulwwiefzojcbxybbruweejw" ]
