module Scrabble where

import Data.Monoid
import Data.Char (toLower)
import Data.List (foldl')

newtype Score = Score { getScore :: Int }
    deriving (Eq, Ord, Show)

instance Monoid Score where
  mempty  = Score 0
  mappend a b = Score $ (getScore a) + (getScore b)

score :: Char -> Score
score a = calcScore $ toLower a
    where calcScore a
            | elem a "aeilnorstu" = Score 1
            | elem a "dg" = Score 2
            | elem a "bcmp" = Score 3
            | elem a "fhvwy" = Score 4
            | elem a "k" = Score 5
            | elem a "jx" = Score 8
            | elem a "qz" = Score 10
            | otherwise = Score 0

scoreString :: String -> Score
scoreString = foldl' sumUp (Score 0)
    where sumUp acc a = acc <> score a