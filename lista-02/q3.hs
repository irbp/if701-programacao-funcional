import Set
import Relation
import Data.List (sort)
import Test.QuickCheck

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = sized genSet

genSet :: (Arbitrary a, Ord a) => Int -> Gen (Set a)
genSet size
    | size > 0 = do x <- arbitrary
                    set' <- genSet (size - 1)
                    return (union (sing x) set')
    | otherwise = return empty

prop_Search :: (Ord a) => Relation a -> a -> Bool
prop_Search graph elem = sort (depthFirst graph elem) == sort (breadthFirst graph elem)

prop_Search2 :: (Ord a) => Relation a -> a -> Bool
prop_Search2 graph elem = head (depthFirst graph elem) == head (breadthFirst graph elem)