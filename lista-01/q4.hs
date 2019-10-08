import Data.List

data Tree a = Nil
            | Node a (Tree a) (Tree a)
               

newtype Set a = Set ( Tree a )

empty :: Set a
empty = Set Nil

sing :: a -> Set a
sing x = Set ( Node x Nil Nil)

instance Ord a => Eq ( Set a ) where
    (==) = eqSet

instance Ord a => Eq (Tree a) where
    Nil == Nil = True
    Nil == _     = False
    _ == Nil     = False
    Node x xl xr == Node y yl yr = prepSet ( treeToList ( Node x xl xr ) ) == prepSet ( treeToList ( Node y yl yr ) )

-- Removes the duplicate elements
prepSet :: Ord a => [ a ] -> [ a ]
prepSet x = map head . group . sort $ x

-- Convertes a Tree in to a list by in order algorithm
treeToList :: Eq a => Tree a -> [ a ]
treeToList ( Nil ) = [  ]
treeToList ( Node x Nil Nil ) = [ x ]
treeToList ( Node x ( Node y Nil Nil ) xr ) = y : x : treeToList xr
treeToList ( Node x xl xr ) = treeToList xl ++ [ x ] ++ treeToList xr

-- Returns if a given set is equal to another set
eqSet :: Ord a => Set a -> Set a -> Bool
eqSet ( Set xs ) ( Set ys ) = xs == ys

-- Verifies if a element is part of a tree
memSet :: Ord a => Set a -> a -> Bool 
memSet ( Set ( Nil ) ) _ = False
memSet ( Set ( Node x xl xr ) ) y
    | x == y = True
    | x < y = memSet ( Set xr ) y
    | otherwise = memSet ( Set xl ) y

-- Put two trees together 
union :: Ord a => Set a -> Set a -> Set a 
union ( Set xs ) ( Set ys ) = Set ( insertTree xs ( treeToList ys ) )

-- Insert each element from a given list in a tree
insertTree :: Ord a => Tree a -> [ a ] -> Tree a
insertTree t [] = t 
insertTree ( Nil ) ( y:ys ) = insertTree ( Node y Nil Nil ) ys
insertTree ( Node x xr xl ) ( y:ys )
    | x == y = insertTree ( Node x xr xl ) ys
    | x < y = insertTree ( Node x xl ( insertTree xr [ y ] ) ) ys
    | otherwise = insertTree ( Node x ( insertTree xl [ y ] ) xr ) ys

-- Convert two trees to lists, put them together and take one representative of the duplicate elements
inter :: Ord a => Set a -> Set a -> Set a
inter ( Set s1 ) ( Set s2 ) = Set $ insertTree ( Nil ) [head x | x <- groupByDuplicates, length x > 1] 
    where treeTogether = ( treeToList s1 ) ++ ( treeToList s2 )
          groupByDuplicates = group . sort $ treeTogether

-- Since insertTree ignores elements who is already in the tree, makeSet just converts a tree in to a list and insert  each one of these elements on the tree
makeSet :: Ord a => Tree a -> Set a
makeSet t = Set ( insertTree ( Nil ) $ map head . group . sort $ treeToList t ) 

-- Just converts a tree in to a list and returns it length
card :: Eq a => Set a -> Int
card ( Set t ) = length $ treeToList t

-- Convert two trees to lists, put them together and take only the unique ones
diff :: Ord a => Set a -> Set a -> Set a
diff ( Set s1 ) ( Set s2 ) = Set $ insertTree ( Nil ) [head x | x <- groupByDuplicates, length x == 1]
    where treeTogether = ( treeToList s1 ) ++ ( treeToList s2 )
          groupByDuplicates = group . sort $ treeTogether

-- Takes two sets and see if the first is inside the second by converting them to lists and using subS
subSet :: Ord a => Set a -> Set a -> Bool
subSet ( Set s1 ) ( Set s2 ) = subS ( treeToList s1 ) ( treeToList s2 )

-- Takes two lists and sees if the first is inside the second
subS :: Eq a => [ a ] -> [ a ] -> Bool
subS [  ] _ = True 
subS ( x:xs ) ys = not ( elem x ys ) && subS xs ys 

-- Converts a tree to a list ant maps it with a given function
mapSet :: ( Ord b, Eq a ) => (a -> b) -> Set a -> Set b
mapSet f ( Set t ) = Set $ insertTree ( Nil ) ( map f $ treeToList t ) 

flatten :: Eq a => Set a -> Tree a
flatten ( Set t ) = t

filterSet :: ( Eq a, Ord a ) => (a -> Bool) -> Set a -> Set a
filterSet f ( Set t ) = Set $ insertTree ( Nil ) ( filter f $ treeToList t )

foldSet :: Eq a => (a -> a -> a) -> a -> Set a -> a
foldSet f x ( Set t ) = foldl f x $ treeToList t 

showSet :: ( Eq a ) => (a -> String ) -> Set a -> String
showSet f ( Set t )  = concat ( map ( ( ++ "\n" ) . f ) ( treeToList t ) ) 


ex1 = ( Node 1 ( Node 2 ( Node 4 Nil Nil ) ( Node 5 Nil Nil ) ) ( Node 3 ( Node 6 Nil Nil ) ( Node 7 Nil Nil ) ) )

ex2 = ( Node 1 ( Node 2 ( Node 4 ( Node 1 Nil Nil ) Nil ) ( Node 5 Nil Nil ) ) ( Node 3 ( Node 6 Nil Nil ) ( Node 7 Nil Nil ) ) )

ex3 = ( Node 1 ( Node 2 ( Node 4 Nil Nil ) ( Node 5 Nil Nil ) ) ( Node 3 ( Node 6 Nil Nil ) Nil ) )

ex4 = ( Node 2 ( Node 4 Nil Nil ) ( Node 5 Nil Nil ) )
