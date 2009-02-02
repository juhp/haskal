------------------------------------------------------------------------
-- |
-- Module      : Haskal.Programs
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

module Haskal.Programs
  ( ProgramInfo ( .. )
  , getExecutables
  , prettyProgram
  , prettyPrograms
  ) where

import qualified Haskal.Trie as Trie
import Control.Monad
import System.Environment
import System.Directory
import Text.PrettyPrint.HughesPJ
import Data.Char
import Haskal.Util
import Haskal.Path

data ProgramInfo = ProgramInfo
  { prinfoName   :: String
  , prinfoPath   :: String
  , prinfoHsName :: String
  } deriving Show

getExecutables :: IO (Trie.Trie Char ProgramInfo)
getExecutables =
  getEnv "PATH" >>= foldM doDir Trie.empty . reverse . split (':'==)
  where
    doDir m dir = (do
      files <-  liftM (zip $ repeat dir) $ getDirectoryContents dir
      filterM isExecutable files >>= foldM insertIt m) `catch` const (return m) 
    insertIt m (d, f) = return $ Trie.insert hsn prinfo m
      where
        hsn = hsName f
        prinfo = ProgramInfo { prinfoName   = f
                             , prinfoPath   = d
                             , prinfoHsName = hsn
                             }


isExecutable :: (String, String) -> IO Bool
isExecutable (dir, file) =
  liftM executable (getPermissions (dir ++ "/" ++ file))
    `catch` const (return False)

prettyPrograms :: Trie.Trie Char ProgramInfo -> Doc
prettyPrograms m = header $+$ contents
  where
    header   = text "module P where\nimport Haskal.Command\n"
    contents = vcat $ map (uncurry prettyProgram) $ Trie.assocs m

prettyProgram :: String -> ProgramInfo -> Doc
prettyProgram hsn (ProgramInfo { prinfoName = nam, prinfoPath = path }) =
  hsep [text $ hsn, equals, text "mkProgram", fileName]
  where
    fileName = doubleQuotes $ text $ concatMap charLit (path </> nam)
    charLit c
      | isPrint c = [c]
      | otherwise = "\\" ++ show (ord c)

hsName :: String -> String
hsName [] = "p"
hsName (c:cs)
  | isLower c = fixup $ concatMap munge (c:cs)
  | otherwise = fixup $ concatMap munge ('p':c:cs)
  where
    fixup s = if s `elem` reserved then 'p':s else s
    munge c'
      | isAlphaNum c' || c' `elem` "'_" = [c']
      | otherwise                     = "_"
    reserved = ["case", "class", "data", "default", "deriving", "do", "else",
                "if", "import", "in", "infix", "infixl", "infixr", "instance",
                "let", "module", "newtype", "of", "then", "type", "where" ] 
