{-# LANGUAGE TemplateHaskell #-}
module TestsMap where

import Test.QuickCheck
import Map
import RBT

prop_singleton = (Map.get 1 $ (Map.singleton 1 2)) == Just 2

prop_get = (Map.get 1 $ Map.singleton 1 2) == Just 2

prop_null = (Map.null $ Map.singleton 1 2) == False

prop_empty = Map.null Map.empty == True

prop_put = (Map.get 3 $ Map.put 3 4 $ Map.singleton 1 2) == Just 4

prop_delete = (Map.get 3 $ Map.delete 3 $ Map.singleton 3 4) == Nothing

prop_from_list = (Map.get 1 $ Map.fromList [(1,2)]) == Just 2

prop_to_list = (Map.toList $ Map.put 3 4 $ Map.singleton 1 2) == [(1,2), (3,4)]

return []
runQuickCheckTests = $quickCheckAll
