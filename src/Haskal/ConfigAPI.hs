------------------------------------------------------------------------
-- |
-- Module      : Haskal.ConfigAPI
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

module Haskal.ConfigAPI
  ( Config ( .. )
  , defaultConfig
  , currentDir
  , mkPrompt
  , at
  , colon
  , rangle
  , user
  , host
  ) where

import System.Directory          ( getCurrentDirectory, getHomeDirectory )
import System.Posix.User         ( getEffectiveUserName )
import System.Posix.Unistd       ( getSystemID, nodeName )
import Text.PrettyPrint.HughesPJ ( Doc, text, hcat )
import Data.List                 ( isPrefixOf )
import Control.Monad             ( liftM )


data Config = Config
  { prompt  :: IO Doc
  , dir     :: String
  , verbose :: Bool
  }

defaultConfig :: Config
defaultConfig = Config
  { prompt  = mkPrompt [user, at, host, colon, currentDir, rangle]
  , dir     = "~/.haskal"
  , verbose = False
  }

mkPrompt :: [IO Doc] -> IO Doc
mkPrompt = liftM hcat . sequence

at :: IO Doc
at = return $ text "@"

colon :: IO Doc
colon = return $ text ":"

rangle :: IO Doc
rangle = return $ text ">"

currentDir :: IO Doc
currentDir = do
  curDir  <- getCurrentDirectory
  homeDir <- getHomeDirectory
  if homeDir `isPrefixOf` curDir
    then return $ text $ '~' : drop (length homeDir) curDir
    else return $ text curDir

user :: IO Doc
user = liftM text getEffectiveUserName

host :: IO Doc
host = liftM (text . nodeName) $ getSystemID
