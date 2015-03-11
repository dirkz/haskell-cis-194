module JoinList where

import Data.Monoid
import Sized
import Debug.Trace (trace)

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- exercise 1

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) left@(Single m1 _) right@(Single m2 _) =
    Append (m1 <> m2) left right

-- exercise 2

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

getSizeInt :: Sized a => a -> Int
getSizeInt = getSize . size

indexJ :: (Show a, Show b, Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i l | trace ("indexJ " ++ show i ++ " " ++ show l) False = undefined
indexJ _ Empty = Nothing
indexJ n (Single m a)
    | n == 0 = Just a
    | otherwise = Nothing
indexJ n (Append m l1 l2)
    | n >= getSizeInt m = Nothing
    | otherwise =
        let s1 = getSizeInt $ tag l1
            s2 = getSizeInt $ tag l2
        in
            if n < s1 then indexJ n l1
            else if n < (s1 + s2) then indexJ n l2 else Nothing

sampleJList :: JoinList Size Char
sampleJList = (Append 4
                (Append 2 (Single 1 'a') (Single 1 'b'))
                (Append 2 (Single 1 'c') (Single 1 'd')))