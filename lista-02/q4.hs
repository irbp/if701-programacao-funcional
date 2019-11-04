data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving (Show)

instance Functor Expr where
    fmap _ (Val x) = Val x
    fmap f (Var x) = Var (f x)
    fmap f (Add x y) = Add (fmap f x) (fmap f y)

instance Applicative Expr where
    pure = Var
    (<*>) (Val x) _ = Val x
    (<*>) _ (Val x) = Val x
    (<*>) (Var f) (Var x) = Var (f x)
    (<*>) (Var f) (Add x y) = Add (fmap f x) (fmap f y)
    (<*>) (Add f g) x = Add (f <*> x) (g <*> x)

instance Monad Expr where
    return = Var
    (>>=) (Val x) _ = Val x
    (>>=) (Var x) f = f x
    (>>=) (Add x y) f = Add (x >>= f) (y >>= f)