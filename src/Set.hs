module Set (Set,
            Set.null,
            empty,
            singleton,
            root,
            Set.min,
            Set.max,
            search,
            insert,
            remove,
            fromList,
            toList) where

import qualified Data.Foldable as F
import qualified Data.Traversable as T


data Color = Red | Black deriving (Show, Eq)
data RBTree a = Empty | Node Color a (RBTree a) (RBTree a) deriving (Eq, Show)

--instance Show a => Show (RBTree a) where
--    show = show . toList

instance Functor RBTree where
    fmap _ Empty = Empty
    fmap f (Node col x left right) = Node col (f x) (fmap f left) (fmap f right)


instance F.Foldable RBTree where
    foldMap _ Empty = mempty
    foldMap f (Node _ x left right) = foldMap f left `mappend` f x `mappend` foldMap f right


type Set = RBTree


null :: Set a -> Bool
null Empty = True
null _     = False


empty :: Set a
empty = Empty


singleton :: a -> Set a
singleton x = Node Black x Empty Empty


root :: Set a -> Maybe a
root Empty          = Nothing
root (Node _ x _ _) = Just x


min :: Set a -> Maybe a
min Empty              = Nothing
min (Node _ x Empty _) = Just x
min (Node _ _ left _)  = Set.min left


max :: Set a -> Maybe a
max Empty              = Nothing
max (Node _ x _ Empty) = Just x
max (Node _ _ _ right) = Set.max right


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


fromList :: Ord a => [a] -> Set a
fromList = foldr insert empty


toList :: Set a -> [a]
toList = F.foldr (:) []
