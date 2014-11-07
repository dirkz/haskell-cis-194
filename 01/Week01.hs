-- exercise 1

import Data.Char

toDigits :: Integer -> [Integer]
toDigits n
  | n > 0 = map ((\n -> n - 48) . toInteger . ord) $ show n
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = fst $ foldr (\x (acc, flag) -> if flag then (2 * x : acc, not flag)
                                                     else (x : acc, not flag)) ([], False) xs
-- exercise 3

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ map (sum . toDigits) xs

-- exercise 4

wholeSum :: Integer -> Integer
wholeSum = sumDigits . doubleEveryOther . toDigits

validate :: Integer -> Bool
validate n = (mod (wholeSum n) 10) == 0

-- exercise 5

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n - 1) a c b) ++ [(a, b)] ++ (hanoi (n - 1) c b a)
