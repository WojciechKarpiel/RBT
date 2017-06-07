{-# LANGUAGE TemplateHaskell #-}

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.QuickCheck
import Test.HUnit hiding (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- import Map
import RBT


--prop_singleton = (Map.get 1 $ (Map.singleton 1 2)) == Just 2

--prop_get = (Map.get 1 $ Map.singleton 1 2) == Just 2

--prop_null = (Map.null $ Map.singleton 1 2) == False

--prop_empty = Map.null Map.empty == True

--prop_put = (Map.get 3 $ Map.put 3 4 $ Map.singleton 1 2) == Just 4

--prop_delete = (Map.get 3 $ Map.delete 3 $ Map.singleton 3 4) == Nothing

--prop_from_list = (Map.get 1 $ Map.fromList [(1,2)]) == Just 2

--prop_to_list = (Map.toList $ Map.put 3 4 $ Map.singleton 1 2) == [(1,2), (3,4)]

--return []
--runQuickCheckTests = $quickCheckAll



main :: IO ()
main = defaultMain [testNullSuite, testSearchSuite, testToFromListSuite, testModifyTreeSuite]


testNullSuite :: Test
testNullSuite = testGroup "Testing RBT null function"
    [testCase "Testing null on empty" (
        assertEqual "Should return true for Empty Node" True (RBT.null empty)),
     testCase "Testing null on non-empty tree" (
        assertEqual "Should return false for nonEmpty Node" False (RBT.null $ singleton 1))]


testSearchSuite :: Test
testSearchSuite = testGroup "Testing various searching functions"
    [testCase "Testing ordinary search with searched element being in tree" (
        assertEqual "Should return searched element from tree" (Just 2) (search 2 $ insert 3 $ insert 2 $ singleton 1)),
    testCase "Testing ordinary search with searched element not being in tree" (
        assertEqual "Should return Nothing if element does not appear in tree" Nothing (search 4 $ insert 3 $ insert 2 $ singleton 1)),
    testCase "Testing searching for min element" (
        assertEqual "Should return minimal element of tree" (Just 1) (RBT.min $ insert 3 $ insert 2 $ singleton 1)),
    testCase "Testing searching for max element" (
        assertEqual "Should return maximal element of tree" (Just 3) (RBT.max $ insert 3 $ insert 2 $ singleton 1)),
    testCase "Testing searching for root element" (
        assertEqual "Should return root for three element RBT" (Just 2) (root $ insert 3 $ insert 2 $ singleton 1))]


testToFromListSuite :: Test
testToFromListSuite = testGroup "Testing creating RBT from list and other way around"
    [testCase "Testing fromList" (
        assertEqual "Should create new tree from list" (Node Black 2 (Node Red 1 Empty Empty) (Node Red 3 Empty Empty)) (fromList [1,2,3])),
    testCase "Testing toList" (
        assertEqual "Should return list from provided RBT" [1,2,3] (toList $ insert 3 $ insert 2 $ singleton 1))]


testModifyTreeSuite :: Test
testModifyTreeSuite = testGroup "Testing function that modify tree like insertion and deletion"
    [testCase "Testing insertion" (
        assertEqual "Should add new element to tree" (Node Black 1 Empty (Node Red 3 Empty Empty)) (insert 3 $ singleton 1)),
    testCase "Testing basic deletion" (
        assertEqual "Should remove element from tree" (Node Black 1 Empty (Node Red 3 Empty Empty)) (remove 2 $ insert 3 $ insert 2 $ singleton 1))]

