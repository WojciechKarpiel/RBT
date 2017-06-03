module RBT (isEmpty,
            singleton,
            insert) where

import qualified Data.Foldable as F
import qualified Data.Traversable as T


data Color = Red | Black deriving Show
data RBTree a = Empty | Node Color a (RBTree a) (RBTree a) deriving Show

type Set = RBTree


isEmpty :: Set a -> Bool
isEmpty Empty = True
isEmpty _     = False


singleton :: a -> Set a
singleton x = Node Black x Empty Empty


search :: (Ord a) => a -> Set a -> Maybe a
search x Empty = Nothing
search x (Node _ y left right)
    | x == y    = Just y
    | x < y     = search x left
    | otherwise = search x right


member :: (Ord a) => a -> Set a -> Bool
member a s = case search a s of
                 Nothing -> False
                 _       -> True


insert :: Ord a => a -> Set a -> Set a
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


balance :: a -> Set a -> Set a -> Set a
balance a (Node Red b bl@(Node Red x xl xr) br) (Node Red c cl cr) = Node Red a (Node Black b bl br) (Node Black c cl cr)
balance a (Node Red b bl br@(Node Red x xl xr)) (Node Red c cl cr) = Node Red a (Node Black b bl br) (Node Black c cl cr)
balance a (Node Red b bl br) (Node Red c cl@(Node Red x xl xr) cr) = Node Red a (Node Black b bl br) (Node Black c cl cr)
balance a (Node Red b bl br) (Node Red c cl cr@(Node Red x xl xr)) = Node Red a (Node Black b bl br) (Node Black c cl cr)

balance a (Node Red b bl (Node Red x xl xr)) ar = Node Black x (Node Red b bl xl) (Node Red a xr ar)
balance a al (Node Red c (Node Red x xl xr) cr) = Node Black x (Node Red a al xl) (Node Red c xr cr)

balance a (Node Red b bl@(Node Red x xl xr) br) ar = Node Black b bl (Node Red a br ar)
balance a al (Node Red c cl cr@(Node Red x xl xr)) = Node Black c (Node Red a al cl) cr

balance a l r = Node Black a l r


-- TODO implement
remove :: Ord a => a -> Set a -> Set a
remove x s = s


instance Functor RBTree where
    fmap _ Empty = Empty
    fmap f (Node col x left right) = Node col (f x) (fmap f left) (fmap f right)


instance F.Foldable RBTree where
    foldMap _ Empty = mempty
    foldMap f (Node _ x left right) = foldMap f left `mappend` f x `mappend` foldMap f right



newtype Pair a b = Pair (a, b) deriving Show

instance Eq a => Eq (Pair a b) where
    Pair (a, b) == Pair (c, d) = a == c

instance Ord a => Ord (Pair a b) where
    Pair (a, b) <= Pair (c, d) = a <= c


newtype Map a b = Map (Set (Pair a b)) deriving Show


isEmpty' :: Map a b -> Bool
isEmpty' (Map Empty) = True
isEmpty' _             = False


empty :: Map a b
empty = Map Empty


get :: Ord a => a -> Map a b -> Maybe b
get x (Map Empty) = Nothing
get x (Map t@(Node _ (Pair (_, z)) _ _)) = search (Pair (x, z)) t >>= \(Pair (_, a)) -> Just a


put :: Ord a => a -> b -> Map a b -> Map a b
put k v (Map t) = Map $ insert (Pair (k, v)) t


delete :: Ord a => a -> Map a b -> Map a b
delete _ m@(Map Empty) = m
delete x (Map t@(Node _ (Pair (_, z)) _ _)) = Map $ remove (Pair (x, z)) t


fromList :: Ord a => [(a, b)] -> Map a b
fromList = foldl (\acc (a, b) -> put a b acc) empty


toList :: Map a b -> [(a, b)]
toList (Map t) = F.foldr (\(Pair (a, b)) acc -> (a, b):acc) [] t
