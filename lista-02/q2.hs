import Test.QuickCheck
import Data.Set

-- Basic properties of unions
prop_UnionSet1 :: Ord a => Set a -> Set a -> Bool
prop_UnionSet1 a b = (a `union` b) == (b `union` a)

prop_UnionSet2 :: Ord a => Set a -> Set a -> Set a -> Bool
prop_UnionSet2 a b c = (a `union` (b `union` c)) == ((a `union ` b) `union` c)

prop_UnionSet3 :: Ord a => Set a -> Set a -> Bool
prop_UnionSet3 a b = a `isSubsetOf` (a `union` b)

prop_UnionSet4 :: Ord a => Set a -> Bool
prop_UnionSet4 a = (a `union` a) == a

prop_UnionSet5 :: Ord a => Set a -> Set a -> Property
prop_UnionSet5 a b = 
    b == empty ==> (a `union` b) == a

prop_UnionSet6 :: Ord a => Set a -> Set a -> Property
prop_UnionSet6 a b = 
    (a `union` b) == b ==> a `isSubsetOf` b

-- Basic properties of intersections
prop_InterSet1 :: Ord a => Set a -> Set a -> Bool
prop_InterSet1 a b = (a `intersection` b) == (b `intersection` a)

prop_InterSet2 :: Ord a => Set a -> Set a -> Set a -> Bool
prop_InterSet2 a b c = a `intersection` (b `intersection` c) == (a `intersection` b) `intersection` c

prop_InterSet3 :: Ord a => Set a -> Set a -> Bool
prop_InterSet3 a b = (a `intersection` b) `isSubsetOf` a

prop_InterSet4 :: Ord a => Set a -> Bool
prop_InterSet4 a = (a `intersection` a) == a

prop_InterSet5 :: Ord a => Set a -> Set a -> Property
prop_InterSet5 a b = 
    b == empty ==> (a `intersection` b) == empty

prop_InterSet6 :: Ord a => Set a -> Set a -> Property
prop_InterSet6 a b = 
    (a `intersection` b) == a ==> a `isSubsetOf` b