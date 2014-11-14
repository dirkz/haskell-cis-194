module Golf where

import Data.List (zip, sort, group)
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

-- get a list of tuples of frequencies of all elements in xs
freq :: Ord a => [a] -> Map.Map a Integer
-- foldr over the list, using an empty association map as starting accumulator,
-- and using insertWith to add 1 to existing counts
freq xs = foldr (\x list -> Map.insertWith (+) x 1 list) (Map.fromList []) xs

-- returns the frequencey of a given number, or 0
--frequency :: Ord a => Map.Map a Integer -> Integer -> a
--frequency freq n = 

-- get a string of '*' for the given level, given a frequency map
--histString :: Map.Map a Integer -> Integer -> String
--histString freq lvl = map () [0,1..9]

-- returns an array of strings with '*' for each level (9-0)
--histStrings :: Map.Map a Integer -> [String]
--histStrings freq = map () [9,8..0]

histogram :: [Integer] -> String
histogram xs = let f = freq xs in
  "h"
