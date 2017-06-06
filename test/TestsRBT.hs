module TestsRBT where

import Test.HUnit
import RBT

testNullForEmptyNode :: Test
testNullForEmptyNode =
  TestCase $ assertEqual "Should return true for Empty Node"
    True (RBT.null empty)

testNullForSingleton :: Test
testNullForSingleton =
  TestCase $ assertEqual "Should return false for nonEmpty Node"
    False (RBT.null $ singleton 1)

testIsBlackForRoot :: Test
testIsBlackForRoot =
  TestCase $ assertEqual "Should return true for root"
    True (RBT.isBlack $ singleton 1)

testToList :: Test
testToList =
  TestCase $ assertEqual "Should return list from provided RBT"
    [1,2,3] (toList $ insert 3 $ insert 2 $ singleton 1)

testRoot :: Test
testRoot =
  TestCase $ assertEqual "Should return root for three element RBT"
    (Just (2)) (root $ insert 3 $ insert 2 $ singleton 1)

testMin :: Test
testMin =
  TestCase $ assertEqual "Should return minimal element of tree"
  (Just(1)) (RBT.min $ insert 3 $ insert 2 $ singleton 1)

testMax :: Test
testMax =
  TestCase $ assertEqual "Should return maximal element of tree"
    (Just(3)) (RBT.max $ insert 3 $ insert 2 $ singleton 1)

testSearch :: Test
testSearch =
  TestCase $ assertEqual "Should return searched element from tree"
    (Just(2)) (search 2 $ insert 3 $ insert 2 $ singleton 1)

testSearch2 :: Test
testSearch2 =
  TestCase $ assertEqual "Should return Nothing if element does not appear in tree"
    Nothing (search 4 $ insert 3 $ insert 2 $ singleton 1)

testInsert :: Test
testInsert =
    TestCase $ assertEqual "Should add new element to tree"
      (Node Black 1 Empty (Node Red 3 Empty Empty)) (insert 3 $ singleton 1)

testRemove :: Test
testRemove =
  TestCase $ assertEqual "Should remove element from tree"
    (Node Black 1 Empty (Node Red 3 Empty Empty)) (remove 2 $ insert 3 $ insert 2 $ singleton 1)

testFromList :: Test
testFromList =
  TestCase $ assertEqual "Should create new tree from list"
    (Node Black 2 (Node Red 1 Empty Empty) (Node Red 3 Empty Empty)) (fromList [1,2,3])



runHunitTests :: IO Counts
runHunitTests = runTestTT $ TestList [
                            testNullForEmptyNode,
                            testNullForSingleton,
                            testIsBlackForRoot,
                            testToList,
                            testRoot,
                            testMin,
                            testMax,
                            testSearch,
                            testSearch2,
                            testInsert,
                            testRemove,
                            testFromList
                            ]
