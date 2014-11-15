module Golf where

import Data.List (zip, sort, group, intercalate)
import qualified Data.Map as Map

-- exercise 1

-- returns a list of every nth element of a given list
everyN :: Integral b => b -> [a] -> [a]
everyN _ [] = []
-- zip xs with their indices and filter only those indices that are multiples of n,
-- then map back to xs (without index)
everyN n xs = map fst $ filter (\(a,b) -> if b `mod` n == 0 then True else False) $ zip xs [1..]

skips :: [a] -> [[a]]
-- call everyN for all numbers from 1 to the input list length,
-- and return as a list
skips xs = map (\l -> everyN l xs) [1..length xs]

-- exercise 2

-- partitions a list into n-tuples
nTuples :: Int -> [a] -> [[a]]
nTuples _ [] = []
nTuples n xs = filter (\ys -> length ys == n) $ (take n xs) : nTuples n (drop 1 xs)

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (\(a:b:ys) -> b) $ filter (\(a:b:c:ys) -> b > a && b > c) $ nTuples 3 xs

-- exercise 3

-- get an association map of numbers to their frequencies
freq :: [Integer] -> Map.Map Integer Integer
-- foldr over the list, using an empty association map as starting accumulator,
-- and using insertWith to add 1 to existing counts
freq xs = foldr (\x list -> Map.insertWith (+) x 1 list) (Map.fromList []) xs

-- returns '*' or ' ', depending on whether the given Maybe frequency
-- has reached the given target frequency
asterisk :: Integer -> Maybe Integer -> Char
asterisk l Nothing = ' '
asterisk l (Just freq) = if freq >= l then '*' else ' '

-- returns a string of '*' for a given level
levelString :: Map.Map Integer Integer -> Integer -> String
levelString freqs n = map (\col -> asterisk n $ Map.lookup col freqs) [0,1..9]

-- Maps levelString over all frequency-levels, producing a vertical histogram.
-- Then drops empty (space-only) lines, then glues with intercalate and "\n"
hString :: [Integer] -> String
hString xs = let freqs = freq xs in
  (intercalate "\n") . (dropWhile (\s -> s == (levelString (freq []) 0)))
  $ (map (levelString freqs) [9,8..1])

-- Calls hString (which does the actual work) and adds some textual decoration
histogram :: [Integer] -> String
histogram xs = "\n" ++ (hString xs) ++ "\n==========\n0123456789\n"
