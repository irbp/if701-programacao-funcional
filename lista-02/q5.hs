type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

extract :: ST a -> (State -> (a, State))
extract (S x) = x

instance Functor ST where
    -- fmap :: (a -> b) ST a -> ST b
    fmap g st =
        do S $ \s -> let (x, s') = (extract st) s in
            (g x, s')

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S $ \s -> (x, s)
    -- (<*>) ST (a -> b) -> ST a -> ST b
    (<*>) stf stx =
        do S $ \s -> let (f, s') = (extract stf) s in
            let (x, s'') = (extract stx) s' in
                (f x, s'')

instance Monad ST where
    -- (>>=) ST a -> (a -> ST b) -> ST b
    (>>=) st f =
        S $ \s -> let (x, s') = app st s in
            app (f x) s'