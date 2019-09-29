import Data.List

--------------- Encoding ---------------

let2int :: Char -> Int
let2int c = fromEnum c - fromEnum 'a'

int2let :: Int -> Char
int2let n = toEnum (fromEnum 'a' + n)

shift :: Int -> Char -> Char
shift n c | c >= 'a' && c <= 'z' = int2let (mod (let2int c + n) 26)
          | otherwise = c

encode :: Int -> String -> String
encode n seq = map (shift n) seq

--------------- Decoding ---------------

table = [ 8.167, 1.492, 2.782, 4.253, 12.702, 2.228, 2.015, 6.094, 6.966, 0.153
        , 0.772, 4.025, 2.406, 6.749, 7.507, 1.929, 0.095, 5.987, 6.327, 9.056
        , 2.758, 0.978, 2.360, 0.150, 1.974, 0.074 ] :: [Float]

percent :: Int -> Int -> Float
percent n1 n2 = (fromIntegral n1 / fromIntegral n2) * 100

onlyLowerCase :: String -> String
onlyLowerCase seq = filter (\x -> x >= 'a' && x <= 'z') seq

countChar :: String -> Char -> Int
countChar seq c = length (filter (== c) seq)

charFreq :: String -> Char -> Float
charFreq seq c = percent (countChar seq c) (length seq)

freqs :: String -> [Float]
freqs seq = map (charFreq (onlyLowerCase seq)) ['a'..'z']

calcChi :: Float -> Float -> Float
calcChi os es = ((os - es) ^ 2) / es

chisqr :: [Float] -> [Float] -> Float
chisqr fo fe = sum (map (\(os, es) -> calcChi os es) (zip fo fe))

rotate :: Int -> [a] -> [a]
rotate 0 xs = xs
rotate n xs = rotate (n - 1) (tail xs ++ [head xs])

calcFactor :: String -> Maybe Int
calcFactor seq = elemIndex (minimum chiList) chiList
    where chiList = [chisqr (rotate n table') table | n <- [0..25]]
          table' = freqs seq

crack :: String -> String
crack seq = 
    case calcFactor seq of
        Just f -> encode (26 - f) seq
        Nothing -> seq