{-|
Module      : Map
Description : Map data struct implementation
Copyright   : Bartosz Walkowicz, Michał Kawałek (c) 2017
License     : BSD 3-clause
Maintainer  : wallko1@hotmail.com
Stability   : experimental

Implementation of Map data struct using Red Black Trees
-}
module Map (Map,
            Map.null,
            empty,
            singleton,
            get,
            put,
            delete,
            fromList,
            toList) where

import qualified RBT as T
import qualified Data.Foldable as F


newtype Pair a b = Pair {getPair :: (a, b)}

instance (Show a, Show b) => Show (Pair a b) where
    show (Pair a) = show a

instance Eq a => Eq (Pair a b) where
    Pair (a, b) == Pair (c, d) = a == c

instance Ord a => Ord (Pair a b) where
    Pair (a, b) <= Pair (c, d) = a <= c


newtype Map a b = Map (T.RBTree (Pair a b)) deriving (Eq, Show)


-- | Time complexity: O(1)
null :: Map a b -> Bool
null (Map s) = T.null s


-- | Time complexity: O(1)
empty :: Map a b
empty = Map T.empty


-- | Time complexity: O(1)
singleton :: a -> b -> Map a b
singleton a b = Map . T.singleton $ Pair (a, b)


-- | Time complexity: O(log n)
get :: Ord a => a -> Map a b -> Maybe b
get x (Map s) = T.root s >>= \(Pair (_, y)) -> T.search (Pair (x, y)) s >>= \(Pair (_, a)) -> Just a


-- | Time complexity: O(log n)
put :: Ord a => a -> b -> Map a b -> Map a b
put k v (Map t) = Map $ T.insert (Pair (k, v)) t


-- | Time complexity: O(log n)
delete :: Ord a => a -> Map a b -> Map a b
delete x m@(Map s) = case T.root s of
                         Nothing            -> m
                         Just (Pair (_, y)) -> Map $ T.remove (Pair (x, y)) s


-- | Time complexity: O(n(log n))
fromList :: Ord a => [(a, b)] -> Map a b
fromList = Map . T.fromList . map Pair


-- | Time complexity: O(n)
toList :: Map a b -> [(a, b)]
toList (Map t) = map getPair . T.toList  $ t
