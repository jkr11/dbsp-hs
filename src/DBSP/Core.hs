module DBSP.Core (
  ZSet(..),
  fromList,
  toList,
  empty,
  (!),
  (<>),
  negateZ
) where

import qualified Data.Map as Map
import Data.Map (Map)

newtype ZSet a w = ZSet (Map a w) 
  deriving (Show, Eq)

instance (Ord a, Num w, Eq w) => Monoid (ZSet a w) where
  mempty = ZSet Map.empty

instance (Ord a, Num w, Eq w) => Semigroup (ZSet a w) where
  (ZSet a) <> (ZSet b) = ZSet $ Map.filter (/= 0) $ Map.unionWith (+) a b

(!) :: (Num w, Ord a) => ZSet a w -> a -> w
(ZSet m) ! k = Map.findWithDefault 0 k m

fromList :: (Ord a, Num w, Eq w) => [(a, w)] -> ZSet a w
fromList = ZSet . Map.filter (/= 0) . Map.fromListWith (+)

toList :: ZSet a w -> [(a, w)]
toList (ZSet m) = Map.toList m

empty :: ZSet a w
empty = ZSet Map.empty

negateZ :: (Num w, Eq w) => ZSet a w -> ZSet a w
negateZ (ZSet m) = ZSet $ Map.map negate $ Map.filter (/= 0) m
