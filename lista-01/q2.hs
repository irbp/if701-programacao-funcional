import Data.Set (fromList, toList)

data Prop = Const Bool | Var Char | And Prop Prop | Imply Prop Prop | Not Prop deriving (Show, Eq)
data Subst = Subst [(Char, Bool)] deriving (Show)

searchTable :: Subst -> Prop -> Prop
searchTable (Subst(x:xs)) (Var a) | fst x == a = Const (snd x)
                                  | otherwise = searchTable (Subst xs) (Var a)

evalAnd :: Prop -> Prop -> Prop
evalAnd a b | a == Const False || b == Const False = Const False
            | a == Const True = b
            | b == Const True = a

evalImply :: Prop -> Prop -> Prop
evalImply a b | a == Const True && b == Const False = Const False
              | otherwise = Const True

evalNot :: Prop -> Prop
evalNot a | a == Const True = Const False
          | otherwise = Const True

eval :: Subst -> Prop -> Prop
eval (Subst s) (Const a) = Const a
eval (Subst s) (Var a) = searchTable (Subst s) (Var a)
eval (Subst s) (And a b) = evalAnd (eval (Subst s) a) (eval (Subst s) b)
eval (Subst s) (Imply a b) = evalImply (eval (Subst s) a) (eval (Subst s) b)
eval (Subst s) (Not a) = evalNot (eval (Subst s) a)

vars :: Prop -> [Char]
vars (Const a) = []
vars (Var a) = [a]
vars (And a b) = (vars a) ++ (vars b)
vars (Imply a b) = (vars a) ++ (vars b)
vars (Not a) = vars a

genCombinations :: Int -> [Bool] -> [[Bool]]
genCombinations n xs | n > 0 = (genCombinations (n - 1) (True : xs))
                                ++ (genCombinations (n - 1) (False : xs))
                     | otherwise = [xs]

bools :: Int -> [[Bool]]
bools n = genCombinations n []

genSubsts :: [Char] -> [[Bool]] -> [Subst]
genSubsts _ [] = []
genSubsts chars (x:xs) = Subst (zip chars x) : genSubsts chars xs

substs :: Prop -> [Subst]
substs p = genSubsts chars bools'
    where chars = toList (fromList (vars p))
          bools' = bools (length chars)

isTaut :: Prop -> Bool
isTaut p | foldl evalAnd (Const True) evalResult == Const True = True
         | otherwise = False
    where substs' = substs p
          evalResult = map (`eval` p) substs'