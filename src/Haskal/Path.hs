------------------------------------------------------------------------
-- |
-- Module      : Haskal.Path
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

module Haskal.Path
  ( Path
  , expand
  , (</>)
  ) where

import System.Directory
import Haskal.Util


type Path = String

expand :: Path -> IO Path
expand ('~':p)   = liftM (++p) getHomeDirectory
expand p@('/':_) = return p
expand p         = liftM (</>p) getCurrentDirectory

(</>) :: Path -> Path -> Path
[] </> p  = p
p  </> [] = p
p  </> q
  | head q == '/' || last p == '/' = p ++ q
  | otherwise                      = p ++ "/" ++ q
