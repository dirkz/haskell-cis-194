module JoinList where

import Data.Monoid
import Sized

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

sizeInt :: Sized a => a -> Int
sizeInt = getSize . size

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i l = search 0 i l
    where
        search :: Sized b => Int -> b -> JoinList b a
        search _ _ Empty = Nothing
        search index i (Single m a)
            | index == i = Just a
            | otherwise = Nothing
        search index i (Append m left right)
            | index < tag left = search index i left
            | index < tag right = search index i right
            | otherwise = Nothing

indexedJList :: JoinList Integer Char
indexedJList = (Append 4 (Append 2 (Single 1 'a') (Single 1 'b')) (Append 2 (Single 1 'c') (Single 1 'd')))
