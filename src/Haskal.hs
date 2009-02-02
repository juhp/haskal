------------------------------------------------------------------------
-- |
-- Module      : Main
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

module Main
  ( ) where

import Control.Monad.State
import System.Eval.Haskell
import System.Plugins
import qualified System.Console.Readline as RL
import Text.PrettyPrint.HughesPJ
import System.IO ( hFlush, stdout )
import System.Directory
import Data.Char
import Codec.Binary.UTF8.String (encodeString, decodeString)
import Haskal.TypeOf ( typeOf_ )

import Haskal.Path ( (</>), expand )


import Haskal.Shell
import Haskal.Programs
import Haskal.Config ( configuration )

import Haskal.ConfigAPI ( Config )
import qualified Haskal.ConfigAPI  as Config
import Haskal.Util

import Haskal.Trie ( Trie )
import qualified Haskal.Trie as Trie

data MainState = MainState
  { config    :: Config
  , configMod :: Module
  , imports   :: [String]
  , progs     :: Trie Char ProgramInfo
  }

newtype MainM a = MainM (StateT MainState Shell a)
  deriving (Monad, MonadIO, MonadState MainState)

main :: IO ()
main = readlineInit >> runMainM (reloadPrograms >> mainLoop)

io :: IO a -> MainM a
io = liftIO

shell :: Shell a -> MainM a
shell = MainM . lift

runMainM :: MainM a -> IO a
runMainM (MainM m) = do
  (m', cfg) <- configuration Nothing
  runShell $ evalStateT m $ MainState
    { config    = cfg
    , configMod = m'
    , imports   = ["Haskal.ShellImports"]
    , progs     = Trie.empty
    }

reconfigure :: MainM Config
reconfigure = do
  (m, cfg) <- gets (configMod &&& config) >>= io . configuration . Just
  modify $ \s -> s { config = cfg, configMod = m }
  return cfg


reloadPrograms :: MainM ()
reloadPrograms = do
  ps <- io getExecutables
  modify $ \s -> s { progs = ps }
  io $ RL.setAttemptedCompletionFunction $ Just $ complete ps
  progFile <- gets ((</>"P.hs") . Config.dir . config)
  mStatus <-  io $ make progFile []
  case mStatus of
    MakeSuccess _ _p -> internal ("l P")
    MakeFailure errs -> do
      io (putStrLn "Reloading Failed: " >> mapM_ putStrLn errs)
      rehash

rehash :: MainM ()
rehash = do
  progFile <- gets ((</>"P.hs") . Config.dir . config)
  io $ putStr "Rehashing..." >> hFlush stdout
  ps <- io getExecutables
  io (writeFile progFile $ render $ prettyPrograms ps)
  reloadPrograms
  io $ putStrLn "Done"

mainLoop :: MainM ()
mainLoop = do
  cfg  <- reconfigure
  line <- io (Config.prompt cfg >>= RL.readline . encodeString . show)
  case line of
    Nothing        -> do
      return ()
    Just ""        -> do
      mainLoop
    Just (':':cmd) -> do
      io $ RL.addHistory (':':cmd)
      internal cmd
      mainLoop
    Just cmd       -> do
      io $ RL.addHistory cmd
      run cmd
      mainLoop

internal :: String -> MainM ()
internal s = case Trie.lookupPrefix cmd internalCommands of
  []      -> io $ putStrLn "Unknown command"
  [(_,c)] -> c args
  _       -> io $ putStrLn "Ambiguous command"
  where
    (cmd, args) = span isAlpha $ dropWhile isSpace s

internalCommands :: Trie Char (String -> MainM ())
internalCommands = Trie.fromList
  [ ("load"      , \m    -> modify $ \s -> s { imports = m : imports s })
  , ("jobs"      , \_    -> shell prettyJobs >>= io . print)
  , ("foreground", \jid  -> shell (awaitJob True $ read jid) >> return ())
  , ("background", \jid  -> bgJob jid)
  , ("rehash"    , \_    -> rehash)
  , ("cd"        , \dir  -> io $ changeDir $ dropWhile isSpace dir)
  , ("which"     , \p    -> printWhich p)
  , ("typeOf"    , \expr -> printType expr)
  ] where
      printWhich p = do
        ps <- gets progs
        case Trie.lookup (dropWhile isSpace p) ps of
          Nothing ->  io $ putStrLn ("Unknown command: " ++ p)
          Just nfo -> io $ putStrLn (prinfoPath nfo </> prinfoName nfo)
      printType expr = do
        cfgDir <- gets (Config.dir . config)
        typ <- gets imports >>= io . typeOf_ expr [cfgDir]
        when (not $ null typ) $ io (putStrLn $ expr ++ " :: " ++ typ)
      bgJob sJid = case reads sJid of
                     [(jid, _)]  -> shell $ backgroundJob jid
                     _           -> io $ putStrLn "Illegal job id"


changeDir :: String ->IO ()
changeDir dir = (expand dir >>= setCurrentDirectory) `catch` print

run :: String -> MainM ()
run cmd = do
  let cmd' = "{-# LINE 1 \"<haskal>\" #-}\n" ++ cmd
  (is, cfgDir) <- gets (imports &&& Config.dir . config)
  cmd'' <- io $ unsafeEval_ ("runCommand (" ++ cmd' ++ " )")
                            is ["-i" ++ cfgDir] [] [cfgDir]
  case cmd'' of
    Left errs -> io $ mapM_ putStrLn errs
    Right cmd''' -> shell (startJob cmd True cmd''') >> return ()


readlineInit :: IO ()
readlineInit = do
  RL.setReadlineName "haskal"
  RL.setAttemptedCompletionFunction $ Just $ complete Trie.empty

complete :: Trie Char ProgramInfo -> String -> Int -> Int ->
               IO (Maybe (String, [String]))
complete trie s start _end = case (start, s) of
  (0, ':':s') -> RL.completionMatches s' (\s'' -> liftM (map (':':)) $
                                            generate internalCommands s'')
  (0, _)      -> RL.completionMatches s (generate trie)
  _           -> return Nothing

generate :: Trie Char a -> String -> IO [String]
generate trie s = return $ map ((s++) . fst) $ Trie.lookupPrefix s trie
