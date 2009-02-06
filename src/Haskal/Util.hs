------------------------------------------------------------------------
-- |
-- Module      : Haskal.Util
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

module Haskal.Util
  ( whenM
  , when_
  , swing
  , first
  , second
  , (***)
  , (&&&)
  , liftM
  , foldl'
  ) where

import Control.Monad
import Control.Arrow
import Data.List

whenM :: Monad m => m Bool -> m a -> m ()
whenM cond action = do
  b <- cond
  when b (action >> return ())

when_ :: Monad m => Bool -> m a -> m ()
when_ b m = when b (m >> return ())

swing :: (((a -> b) -> b) -> c -> d) -> c -> a -> d
swing f c a = (f ($ a)) c
