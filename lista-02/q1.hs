import Test.QuickCheck

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized genTree

genTree :: Arbitrary a => Int -> Gen (Tree a)
genTree size
    | size > 0 = do x <- arbitrary
                    l <- genTree (size `div` 2)
                    r <- genTree (size `div` 2)
                    return (Node x l r)
    | otherwise = return EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = Node x EmptyTree EmptyTree
treeInsert x (Node n l r)
    | x == n = Node x l r
    | x < n  = Node n (treeInsert x l) r
    | x > n  = Node n l (treeInsert x r)

isEmptyTree :: Tree a -> Bool
isEmptyTree EmptyTree = True
isEmptyTree (Node _ _ _) = False

getTreeHeight :: Tree a -> Int
getTreeHeight EmptyTree = 0
getTreeHeight (Node _ EmptyTree EmptyTree) = 0
getTreeHeight (Node n l r) = 1 + max (getTreeHeight l) (getTreeHeight r)

getTreeTotNodes :: Tree a -> Int
getTreeTotNodes EmptyTree = 0
getTreeTotNodes (Node n l r) = 1 + getTreeTotNodes l + getTreeTotNodes r

getTreeTotInternalNodes :: Tree a -> Int
getTreeTotInternalNodes EmptyTree = 0
getTreeTotInternalNodes (Node _ EmptyTree EmptyTree) = 0
getTreeTotInternalNodes (Node n l r) = 1 + getTreeTotInternalNodes l + getTreeTotInternalNodes r

getTreeTotLeaves:: Tree a -> Int
getTreeTotLeaves EmptyTree = 0
getTreeTotLeaves (Node _ EmptyTree EmptyTree) = 1
getTreeTotLeaves (Node _ l r) = getTreeTotLeaves l + getTreeTotLeaves r

prop_NodesFullBT :: Tree a -> Property
prop_NodesFullBT tree =
    not (isEmptyTree tree) ==> totNodes >= (2 * height + 1) && totNodes <= (2 ^ (height + 1) - 1)
        where totNodes = getTreeTotNodes tree
              height = getTreeHeight tree

prop_LeavesPerfectBT :: Tree a -> Property
prop_LeavesPerfectBT tree = 
    not (isEmptyTree tree) ==> totLeaves == (totNodes + 1) `div` 2
        where totLeaves = getTreeTotLeaves tree
              totNodes = getTreeTotNodes tree

prop_LeavesFullBT :: Tree a -> Property
prop_LeavesFullBT tree = 
    not (isEmptyTree tree) ==> totNodes == (2 * totLeaves - 1)
        where totNodes = getTreeTotNodes tree
              totLeaves = getTreeTotLeaves tree
-- TODO verificar se uma árvore com apenas um nó é uma árvore balanceada
prop_HeightBalancedTree :: Tree a -> Property
prop_HeightBalancedTree tree =
    not (isEmptyTree tree) ==> height == logLeaves && logLeaves == logHalfNodes && logHalfNodes == logNodes
        where logLeaves = ceiling $ logBase 2 totLeaves + 1
              logHalfNodes = ceiling $ logBase 2 ((totNodes + 1) / 2) + 1
              logNodes = ceiling $ logBase 2 (totNodes + 1)
              totLeaves = fromIntegral $ getTreeTotLeaves tree
              totNodes = fromIntegral $ getTreeTotNodes tree
              height = getTreeHeight tree

prop_PerfectFullBT :: Tree a -> Property
prop_PerfectFullBT tree =
    not (isEmptyTree tree) ==> totLeaves == (2 ^ height) && totNodes == (2 ^ (height + 1) - 1)
        where totNodes = getTreeTotNodes tree
              totLeaves = getTreeTotLeaves tree
              height = getTreeHeight tree

prop_InternalNodesCompleteBT :: Tree a -> Property
prop_InternalNodesCompleteBT tree = 
    not (isEmptyTree tree) ==> totInternalNodes == (floor $ totNodes / 2)
        where totInternalNodes = getTreeTotInternalNodes tree
              totNodes = fromIntegral $ getTreeTotNodes tree