------------------------------------------------------------------------
-- |
-- Module      : Haskal.Trie
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

module Haskal.Trie
  ( Trie
  , empty
  , null
  , insert
  , insertWith
  , lookup
  , lookupPrefix
  , assocs
  , keys
  , elems
  , fromList
  ) where

import Prelude hiding ( null, lookup )
import Data.Maybe     ( fromMaybe, isNothing )
import Control.Monad  ( MonadPlus, mzero, msum, mplus, liftM, guard )
import Control.Arrow  ( first )

import qualified Data.Map as Map

data Trie k v
  = Node (Maybe v) (Map.Map k (Trie k v))
  | Leaf [k] v
    deriving Show

empty :: Trie k v
empty = Node Nothing Map.empty

null :: Trie k v -> Bool
null (Node Nothing m) = Map.null m
null _                = False

insertWith :: Ord k => (v -> v -> v) -> [k] -> v -> Trie k v -> Trie k v
insertWith f [] v (Leaf [] v') = Leaf [] $ f v v'
insertWith _ [] v (Leaf (k:ks) v') = Node (Just v) $ singleLeaf k ks v'
insertWith f [] v (Node (Just v') m) = Node (Just $ f v v') m
insertWith _ [] v (Node Nothing m) = Node (Just v) m
insertWith _ (k:ks) v (Leaf [] v') = Node (Just v') $ singleLeaf k ks v
insertWith f (k:ks) v (Leaf (k':ks') v')
  | k == k' = Node Nothing $ Map.singleton k $ insertWith f ks v $ Leaf ks' v'
  | otherwise = Node Nothing $ Map.insert k (Leaf ks v) $ singleLeaf k' ks' v'
insertWith f (k:ks) v (Node mv m)
  | isNothing mv && Map.null m = Leaf (k:ks) v
  | otherwise               = Node mv (Map.insert k t m)
  where
    t = insertWith f ks v $ fromMaybe empty $ Map.lookup k m

singleLeaf :: k -> [k] -> v -> Map.Map k (Trie k v)
singleLeaf k ks v = Map.singleton k $ Leaf ks v

insert :: Ord k => [k] -> v -> Trie k v -> Trie k v
insert = insertWith const

lookupPrefix :: (MonadPlus m, Ord k) => [k] -> Trie k v -> m ([k], v)
lookupPrefix [] (Leaf ks v)           = return (ks, v)
lookupPrefix [] (Node (Just v) m)     = return ([], v) `mplus` lookupRest m
lookupPrefix [] (Node Nothing m)      = lookupRest m
lookupPrefix (k:ks) (Node _ m) 
    = case Map.lookup k m of
        Just x  -> lookupPrefix ks x
        Nothing -> fail "Trie.lookupPrefix"
lookupPrefix (_:_)  (Leaf [] _)       = mzero
lookupPrefix (k:ks) (Leaf (k':ks') v) = guard (k == k') >>
                                        lookupPrefix ks (Leaf ks' v)

lookupRest :: (MonadPlus m, Ord k) => Map.Map k (Trie k v) -> m ([k], v)
lookupRest = msum . map lookItUp . Map.assocs
  where
    lookItUp (k, t) = liftM (first (k:)) $ lookupPrefix [] t


lookup :: (Monad m, Ord k) => [k] -> Trie k v -> m v
lookup k t = case lookupPrefix k t of
  Just ([], v) -> return v
  _            -> fail "Trie.lookup"

assocs :: Ord k => Trie k v -> [([k], v)]
assocs = lookupPrefix []

elems :: Ord k => Trie k v -> [v]
elems = map snd . assocs

keys :: Ord k => Trie k v -> [[k]]
keys = map fst . assocs

fromList :: Ord k => [([k], v)] -> Trie k v
fromList = foldr (\(k, v) m -> insert k v m) empty
