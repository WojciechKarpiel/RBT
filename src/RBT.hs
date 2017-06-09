{-|
Module      : RBT
Description : RBT data struct implementation
Copyright   : Bartosz Walkowicz, Michał Kawałek (c) 2017
License     : BSD 3-clause
Maintainer  : wallko1@hotmail.com
Stability   : experimental

Implementation of RBT data struct
-}
module RBT (RBTree(..),
            Color(..),
            RBT.null,
            empty,
            singleton,
            root,
            RBT.min,
            RBT.max,
            search,
            insert,
            remove,
            member,
            isBlack,
            isRed,
            fromList,
            toList) where

import qualified Data.Foldable as F
import qualified Data.Traversable as T


data Color = Red | Black deriving (Show, Eq)
data RBTree a = Empty | Node Color a (RBTree a) (RBTree a) deriving (Eq, Show)


instance Functor RBTree where
    fmap _ Empty = Empty
    fmap f (Node col x left right) = Node col (f x) (fmap f left) (fmap f right)


instance F.Foldable RBTree where
    foldMap _ Empty = mempty
    foldMap f (Node _ x left right) = foldMap f left `mappend` f x `mappend` foldMap f right


-- | Time complexity: O(1)
null :: RBTree a -> Bool
null Empty = True
null _     = False


-- | Time complexity: O(1)
empty :: RBTree a
empty = Empty


-- | Time complexity: O(1)
singleton :: a -> RBTree a
singleton x = Node Black x Empty Empty


-- | Time complexity: O(1)
root :: RBTree a -> Maybe a
root Empty          = Nothing
root (Node _ x _ _) = Just x


-- | Time complexity: O(log n)
min :: RBTree a -> Maybe a
min Empty              = Nothing
min (Node _ x Empty _) = Just x
min (Node _ _ left _)  = RBT.min left


-- | Time complexity: O(log n)
max :: RBTree a -> Maybe a
max Empty              = Nothing
max (Node _ x _ Empty) = Just x
max (Node _ _ _ right) = RBT.max right


-- | Time complexity: O(log n)
search :: (Ord a) => a -> RBTree a -> Maybe a
search x Empty = Nothing
search x (Node _ y left right)
    | x == y    = Just y
    | x < y     = search x left
    | otherwise = search x right


-- | Time complexity: O(log n)
member :: (Ord a) => a -> RBTree a -> Bool
member a s = case search a s of
                 Nothing -> False
                 _       -> True


-- | Time complexity: O(1)
isRed :: RBTree a -> Bool
isRed Empty              = False
isRed (Node Black _ _ _) = False
isRed _                  = True


-- | Time complexity: O(1)
isBlack :: RBTree a -> Bool
isBlack = not . isRed


-- | Time complexity: O(log n)
insert :: Ord a => a -> RBTree a -> RBTree a
insert a t = let (Node _ b l r) = _insert a t in Node Black b l r
    where _insert x Empty = Node Red x Empty Empty
          _insert x t@(Node Black y left right)
              | x == y    = Node Black x left right
              | x > y     = balance y left (_insert x right)
              | otherwise = balance y (_insert x left) right
          _insert x (Node Red y left right)
              | x == y    = Node Red x left right
              | x > y     = Node Red y left $ _insert x right
              | otherwise = Node Red y (_insert x left) right


balance :: a -> RBTree a -> RBTree a -> RBTree a
balance a (Node Red b bl br) (Node Red c cl cr)
    | isRed bl || isRed br || isRed cl || isRed cr = Node Red a (Node Black b bl br) (Node Black c cl cr)

balance a (Node Red b bl (Node Red x xl xr)) ar = Node Black x (Node Red b bl xl) (Node Red a xr ar)
balance a al (Node Red c (Node Red x xl xr) cr) = Node Black x (Node Red a al xl) (Node Red c xr cr)

balance a (Node Red b bl@(Node Red x xl xr) br) ar = Node Black b bl (Node Red a br ar)
balance a al (Node Red c cl cr@(Node Red x xl xr)) = Node Black c (Node Red a al cl) cr

balance a l r = Node Black a l r


-- | Time complexity: O(log n)
remove :: Ord a => a -> RBTree a -> RBTree a
remove _ Empty = Empty
remove x t@(Node Black z zl zr) = let s = if isBlack zl && isBlack zr
                                              then Node Red z zl zr
                                              else t
                                  in case _remove x s False of
                                      ((Node _ y l r), _) -> Node Black y l r
                                      (Empty, _)          -> Empty
    where _remove a t@(Node Red b Empty Empty) d
              | a == b || d = (Empty, b)
              | otherwise   = (t, a)
          _remove a t@(Node Black b (Node Red c Empty Empty) Empty) d
              | a == b || d = (Node Black c Empty Empty, b)
              | a == c      = (Node Black b Empty Empty, a)
              | otherwise   = (t, a)
          _remove a t@(Node Black b Empty (Node Red c Empty Empty)) d
              | a == b || d = (Node Black c Empty Empty, b)
              | a == c      = (Node Black b Empty Empty, a)
              | otherwise   = (t, a)
          _remove a t@(Node _ b l r) d =
              let (Node col v vl vr) = case a <= b of
                                           True  -> pushRedToLeft t
                                           False -> pushRedToRight t
              in case a `compare` v of
                  LT -> let (vl', val) = _remove a vl d    in (Node col v vl' vr, val)
                  EQ -> let (vl', val) = _remove a vl True in (Node col val vl' vr, v)
                  GT -> let (vr', val) = _remove a vr d    in (Node col v vl vr', val)


pushRedToRight :: RBTree a -> RBTree a
pushRedToRight (Node Red a (Node Black b bl br) (Node Black c cl cr))
    | isBlack bl && isBlack br && isBlack cl && isBlack cr = Node Black a (Node Red b bl br) (Node Red c cl cr)
pushRedToRight (Node Black a (Node Red b bl br) t@(Node Black c cl cr))
    | isBlack cl && isBlack cr = Node Black b bl (Node Red a br t)
pushRedToRight (Node Red a (Node Black b (Node Red d dl dr) br) (Node Black c cl cr))
    | isBlack cl && isBlack cr = Node Red b (Node Black d dl dr) (Node Black a br (Node Red c cl cr))
pushRedToRight (Node Red a (Node Black b bl (Node Red d dl dr)) (Node Black c cl cr))
    | isBlack cl && isBlack cr = Node Red d (Node Black b bl dl) (Node Black a dr (Node Red c cl cr))
pushRedToRight t = t


pushRedToLeft :: RBTree a -> RBTree a
pushRedToLeft (Node Red a (Node Black b bl br) (Node Black c cl cr))
    | isBlack bl && isBlack br && isBlack cl && isBlack cr = Node Black a (Node Red b bl br) (Node Red c cl cr)
pushRedToLeft (Node Black a t@(Node Black b bl br) (Node Red c cl cr))
    | isBlack bl && isBlack br = Node Black c (Node Red a t cl) cr
pushRedToLeft (Node Red a (Node Black b bl br) (Node Black c (Node Red d dl dr) cr))
    | isBlack bl && isBlack br = Node Red d (Node Black a (Node Red b bl br) dl) (Node Black c dr cr)
pushRedToLeft (Node Red a (Node Black b bl br) (Node Black c cl (Node Red d dl dr)))
    | isBlack bl && isBlack br = Node Red c (Node Black a (Node Red b bl br) cl) (Node Black d dl dr)
pushRedToLeft t = t


-- | Time complexity: O(n(log n))
fromList :: Ord a => [a] -> RBTree a
fromList = foldr insert empty


-- | Time complexity: O(n)
toList :: RBTree a -> [a]
toList = F.foldr (:) []
