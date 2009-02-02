------------------------------------------------------------------------
-- Module      : Haskal.Command
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

module Haskal.Command
  ( Command
  , Program
  , DoIO
  , io
  , Cmd
  , Marshal
  , (-.)
  , ( # )
  , (>|)
  , (&>|)
  , (>|<)
  , mkProgram
  , runCommand
  ) where

import Control.Monad
import System.Posix.Process
import Haskal.Marshal
import Data.Word ( Word8 )
import System.Posix.IO
import System.Posix.Types
import System.IO
import Data.Typeable


infixl 0 >|<
infixl 9 -., #
infixl 8 >|, &>|


-- | An external program which reads a value of type i from standard
-- input and writes values a value of type o to standard output.
data Program i o = Program String [String]


instance Typeable2 Program where
  typeOf2 (Program _ _) = mkTyConApp (mkTyCon "Program") []

-- | Construct an external program located at the given path.
mkProgram :: String -> Program String String
mkProgram p = Program p []

-- | A command which reads an i and prints and o.
newtype Command i o = Command { unCommand :: [IO ()] }

instance Typeable2 Command where
  typeOf2 (Command _) = mkTyConApp (mkTyCon "Command") []

-- | Produce a list of actions which are to be executed in parallel.
runCommand :: (Cmd c, Marshal i, Marshal o) => c i o -> [IO ()]
runCommand = unCommand . toCommand

redirect :: Fd -> String -> IO ()
redirect oldFd file = do
  newFd <- openFile file WriteMode >>= handleToFd
  closeFd oldFd
  newFd `dupTo` oldFd
  return ()

-- | Pipe output from the command to the given file.
(>|) :: (Marshal i, Marshal o, Cmd c) => c i o -> String -> Command i o
c >| file = case reverse $ unCommand $ toCommand c of
  (io:ios) -> Command (reverse ((redirect stdOutput file >> io):ios))

-- | Pipe error output from the command to the given file.
(&>|) :: (Marshal i, Marshal o, Cmd c) => c i o -> String -> Command i o
c &>| file = case reverse $ unCommand $ toCommand c of
  (io:ios) -> Command (reverse ((redirect stdError file >> io):ios))


-- | An io command from i to o
newtype DoIO i o = DoIO (i -> IO o)

instance Typeable2 DoIO where
  typeOf2 (DoIO _) = mkTyConApp (mkTyCon "DoIO") []


-- | Perform an io command.
io :: (i -> IO o) -> DoIO i o
io = DoIO

instance Cmd DoIO where
  toCommand (DoIO f) = Command [ runIt ]
    where
      runIt = do s <- getContentsWord8 >>= f . unmarshal
                 putContentsWord8 $ marshal s []

-- | The class of commands to executed.
class Cmd c where
  toCommand :: (Marshal i, Marshal o) => c i o -> Command i o

instance Cmd Program where
  toCommand (Program p args) = Command [ executeFile p True args Nothing ]

instance Cmd (->) where
  toCommand f = Command [ runIt ]
    where
      runIt = do s <- getContentsWord8 >>= return .f . unmarshal
                 putContentsWord8 $ marshal s []

instance Cmd Command where
  toCommand = id

-- | Arguments that can be given to external programs.
class Argument a where
  toArgument     :: a -> [String]
  listToArgument :: [a] -> [String]

  listToArgument = concatMap toArgument

instance Argument a => Argument [a] where
  toArgument = listToArgument

instance Argument Char where
  toArgument     c = [[c]]
  listToArgument s = words s

instance Argument Int where
  toArgument n = [show n]

instance Argument Integer where
  toArgument n = [show n]

instance Argument Double where
  toArgument d = [show d]

instance Argument (Program i o) where
  toArgument (Program p _) = [reverse $ takeWhile (/='/') $ reverse p]


-- | Append the command line option to the given program. The option
-- will be prependen with "-" so that e.g. ls -. \"l\" corresponds to ls
-- -l
(-.) :: Argument a => Program i o -> a -> Program i o
Program p args -. arg = case toArgument arg of
  []     -> Program p args
  (a:as) -> Program p (args ++ (('-':a):as))

-- | Append the argument to the program's list of arguments.
( # ) :: Argument a => Program i o -> a -> Program i o
Program p args # arg = Program p (args ++ toArgument arg)

-- | Combine two commands in parallel, piping their outputs together
(>|<) :: (Cmd c1, Cmd c2, Marshal t, Marshal i, Marshal o) =>
             c1 i t -> c2 t o -> Command i o
c1 >|< c2 = case (toCommand c1, toCommand c2) of
              (Command cs1, Command cs2) -> Command (cs1 ++ cs2)

getContentsWord8 :: IO [Word8]
getContentsWord8 = liftM (($[]) . marshalList) getContents

putContentsWord8 :: [Word8] -> IO ()
putContentsWord8 xs = putStr (unmarshalList xs) >> hFlush stdout
