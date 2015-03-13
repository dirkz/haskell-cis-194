{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module JoinListBuffer where

import Data.Monoid
import Data.List (foldl')
import Buffer
import JoinList
import Scrabble
import Sized

instance Buffer (JoinList (Score, Size) String) where
    toString Empty = ""
    toString (Single _ s) = s
    toString (Append _ left right) = toString left ++ toString right

    fromString = (foldl' addLine Empty) . lines
        where addLine acc line =
                acc +++ Single (scoreString line, Size 1) line

    line = indexJ

    replaceLine n s Empty = Single (scoreString s, Size 1) s
    replaceLine n s orig@(Single m _)
        | n == 0 = Single (scoreString s, snd m) s
        | otherwise = orig
    replaceLine n s orig@(Append _ left right)
        | n < sizeLeft =
            let
                newLeft = replaceLine n s left
                newTag = (tag newLeft) <> (tag right)
            in Append newTag newLeft right
        | n < (sizeLeft + sizeRight) =
            let
                newRight = replaceLine (n - sizeLeft) s right
                newTag = (tag left) <> (tag newRight)
            in Append newTag left newRight 
        | otherwise = orig
        where
            sizeLeft = getSize $ snd $ tag left
            sizeRight = getSize $ snd $ tag right

    numLines Empty = 0
    numLines (Single _ _) = 1
    numLines (Append _ l r) = numLines l + numLines r

    value Empty = 0
    value (Single t _) = getScore $ fst t
    value (Append t _ _) = getScore $ fst t
