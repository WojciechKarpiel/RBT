module Map (Map,
            Map.null,
            empty,
            singleton,
            get,
            put,
            delete,
            fromList,
            toList) where

import qualified Set as S
import qualified Data.Foldable as F


newtype Pair a b = Pair {getPair :: (a, b)}

instance (Show a, Show b) => Show (Pair a b) where
    show (Pair a) = show a

instance Eq a => Eq (Pair a b) where
    Pair (a, b) == Pair (c, d) = a == c

instance Ord a => Ord (Pair a b) where
    Pair (a, b) <= Pair (c, d) = a <= c


newtype Map a b = Map (S.Set (Pair a b)) deriving (Eq, Show)

--instance (Show a, Show b) => Show (Map a b) where
--    show = show . toList


null :: Map a b -> Bool
null (Map s) = S.null s


empty :: Map a b
empty = Map S.empty


singleton :: a -> b -> Map a b
singleton a b = Map . S.singleton $ Pair (a, b)


get :: Ord a => a -> Map a b -> Maybe b
get x (Map s) = S.root s >>= \(Pair (_, y)) -> S.search (Pair (x, y)) s >>= \(Pair (_, a)) -> Just a


put :: Ord a => a -> b -> Map a b -> Map a b
put k v (Map t) = Map $ S.insert (Pair (k, v)) t


delete :: Ord a => a -> Map a b -> Map a b
delete x m@(Map s) = case S.root s of
                         Nothing            -> m
                         Just (Pair (_, y)) -> Map $ S.remove (Pair (x, y)) s


fromList :: Ord a => [(a, b)] -> Map a b
fromList = Map . S.fromList . map Pair


toList :: Map a b -> [(a, b)]
toList (Map t) = map getPair . S.toList  $ t
