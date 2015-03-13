{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

import Data.Monoid
import Control.Monad
import JoinList
import Sized
import JoinList
import Scrabble
import JoinListBuffer
import Editor
import Buffer

sampleJList1 :: JoinList Size String
sampleJList1 = (Append 4
                (Append 2 (Single 1 "0") (Single 1 "1"))
                (Append 2 (Single 1 "2") (Single 1 "3")))

sampleJList2 :: JoinList Size String
sampleJList2 = (Append 8
                (Append 6
                    (Append 4
                        (Append 2 (Single 1 "0") (Single 1 "1"))
                        (Append 2 (Single 1 "2") (Single 1 "3")))
                    (Append 2 (Single 1 "4") (Single 1 "5")))
                (Append 2 (Single 1 "6") Empty))

checkLookup :: (Sized m, Eq a, Monoid m) => JoinList m a -> Int -> Bool
checkLookup l n = (indexJ n l) == (jlToList l !!? n)

checkDrop :: (Sized m, Eq a, Monoid m) => JoinList m a -> Int -> Bool
checkDrop l n = jlToList (dropJ n l) == drop n (jlToList l)

checkTake :: (Sized m, Eq a, Monoid m) => JoinList m a -> Int -> Bool
checkTake l n = jlToList (takeJ n l) == take n (jlToList l)

main = do
    --putStrLn "JoinList tests"
    --forM_ [1..5] $ print . checkLookup  sampleJList1
    --forM_ [1..8] $ print . checkLookup  sampleJList2
    --forM_ [0..2] $ print . checkDrop (Empty :: JoinList Size String)
    --forM_ [0..2] $ print . checkDrop (Single (Size 1) "0")
    --forM_ [0..5] $ print . checkDrop sampleJList1
    --forM_ [0..8] $ print . checkDrop sampleJList2
    --forM_ [0..6] $ print . checkTake sampleJList1
    --forM_ [0..8] $ print . checkTake sampleJList2
    --print $ scoreLine "yay " +++ scoreLine "haskell!"
    let buffer = fromString "Line 1\nLine 2\nLine 3" :: JoinList (Score, Size) String
    --print $ buffer
    --print $ line 1 buffer
    --print $ replaceLine 1 "hi" buffer
    --print $ replaceLine 0 "hi" (Single (Score 0, Size 0) "what")
    --print $ replaceLine 0 "hi" (Empty :: JoinList (Score, Size) String)
    --print $ replaceLine 1 "hi" (Single (Score 0, Size 0) "what")
    runEditor editor buffer
