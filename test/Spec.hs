{-# LANGUAGE TemplateHaskell #-}

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.QuickCheck
import Test.HUnit hiding (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import RBT


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
        assertEqual "Should remoe element from tree" (Node Black 1 Empty (Node Red 3 Empty Empty)) (remove 2 $ insert 3 $ insert 2 $ singleton 1)),
    testProperty "Testing sequence inserting" (
        forAll (arbitrary ::  Gen (RBTree Int)) (\t -> isRootBlack t && haveRedNodesBlackSons t && areBlackPathsEquals t)),
    testProperty "Testing removal after sequence inserting" (
        forAll (arbitrary ::  Gen (RBTree Int)) (\x -> let t = remove 0 x in isRootBlack t && haveRedNodesBlackSons t && areBlackPathsEquals t)),
    testProperty "Testing removal of element not present in tree" (
        forAll (arbitrary ::  Gen (RBTree Int)) (\x -> let t = remove 10000 x in isRootBlack t && haveRedNodesBlackSons t && areBlackPathsEquals t))]



-- helper functions

instance Monoid Int where
    mempty = 0
    a `mappend` b = a + b


instance (Monoid a, Ord a, Arbitrary a) => Arbitrary (RBTree a) where
    arbitrary = arbitrary >>= return . insert mempty . fromList


isRootBlack :: RBTree a -> Bool
isRootBlack Empty              = True
isRootBlack (Node Black _ _ _) = True
isRootBlack _                  = False


haveRedNodesBlackSons :: RBTree a -> Bool
haveRedNodesBlackSons Empty              = True
haveRedNodesBlackSons (Node Black _ l r) = haveRedNodesBlackSons l && haveRedNodesBlackSons r
haveRedNodesBlackSons (Node Red _ l r)
    | isBlack l && isBlack r = haveRedNodesBlackSons l && haveRedNodesBlackSons r
    | otherwise              = False


areBlackPathsEquals :: RBTree a -> Bool
areBlackPathsEquals x = let (a, b) = _checkPaths x in b
    where _checkPaths Empty = (1, True)
          _checkPaths (Node col _ l r) = let (a1, b1) = _checkPaths l
                                             (a2, b2) = _checkPaths r
                                         in (a1 + if col == Black then 1 else 0, b1 && b2 && (a1 == a2))
