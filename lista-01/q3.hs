a0 = 1.0
dist = 0.00001
aproxList n = iterate (\a -> (a + n / a) / 2) a0

calcDist :: [Double] -> Double
calcDist (x1:x2:xs) | abs (x2 - x1) <= dist = x2
                    | otherwise = calcDist (x2:xs)

sqroot :: Double -> Double
sqroot n = calcDist (aproxList n)