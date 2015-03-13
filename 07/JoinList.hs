module JoinList where

import Data.Monoid
import Sized
import Scrabble

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
l +++ Empty = l
Empty +++ l = l
left +++ right = Append (m1 <> m2) left right
    where m1 = tag left
          m2 = tag right

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

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
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
            else if n < (s1 + s2) then indexJ (n - s1) l2
                    else Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n s@(Single _ _)
    | n == 0 = s
    | otherwise = Empty
dropJ n (Append m l1 l2)
    | n >= sizeM = Empty
    | n == sizeL1 = l2
    | n < sizeL1 = (dropJ n l1) +++ l2
    | otherwise  = (dropJ (n - sizeL1) l2) +++ Empty
    where sizeM = getSizeInt m
          sizeL1 = getSizeInt $ tag l1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n l | n <= 0 = Empty
takeJ n s@(Single _ _) | n >= 1 = s
takeJ n all@(Append m l1 l2)
    | n >= sizeM = all
    | n == sizeL1 = l1
    | n > sizeL1 = l1 +++ (takeJ (n - sizeL1) l2)
    | n < sizeL1 = takeJ n l1
    where sizeM = getSizeInt m
          sizeL1 = getSizeInt $ tag l1

-- exercise 3

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s
