------------------------------------------------------------------------
-- |
-- Module      : Haskal.Marshal
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

module Haskal.Marshal
  ( Marshal ( .. )
  ) where

import Data.Char     ( chr, ord )
import Data.List     ( intersperse )
import Data.Word     ( Word8 )
import Haskal.Util   ( split )

class Marshal a where
  marshal       :: a -> ([Word8] -> [Word8])
  unmarshal     :: [Word8] -> a
  marshalList   :: [a] -> ([Word8] -> [Word8])
  unmarshalList :: [Word8] -> [a]

  marshalList   = foldr (.) id . intersperse (10:) .  map marshal
  unmarshalList = map unmarshal . split (10==)

instance Marshal a => Marshal [a] where
  marshal   = marshalList
  unmarshal = unmarshalList

instance Marshal Word8 where
  marshal       = (:)
  unmarshal     = head
  marshalList   = (++)
  unmarshalList = id

instance Marshal Char where
  marshal       = (:) . fromIntegral . ord
  unmarshal     = chr . fromIntegral . head
  marshalList   = foldr (.) id . map marshal
  unmarshalList = map (chr . fromIntegral)

instance Marshal Int where
  marshal   = marshal . show
  unmarshal = read . unmarshal

instance Marshal Integer where
  marshal   = marshal . show
  unmarshal = read . unmarshal

instance Marshal Float where
  marshal   = marshal . show
  unmarshal = read . unmarshal

instance Marshal Double where
  marshal   = marshal . show
  unmarshal = read . unmarshal

instance Marshal () where
  marshal   = marshal . show
  unmarshal = read . unmarshal
