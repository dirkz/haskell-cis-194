{-# LANGUAGE FlexibleInstances #-}

module Fib where

import Data.Bits

-- exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- exercise 2

fibs2 :: [Integer]
fibs2 = map fst $ iterate stepper (0, 1)
    where stepper (n0, n1) = (n1, n0 + n1)

-- exercise 3

data Stream a = Cons a (Stream a)
    deriving (Ord, Eq)

instance Show a => Show (Stream a) where
    show xs = "Stream " ++ (show $ take 20 $ streamToList xs)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- exercise 4

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a $ streamFromSeed f (f a)

-- exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) (Cons b bs) = Cons a $ Cons b $ interleaveStreams as bs

numZeroRightBits :: Integer -> Integer
numZeroRightBits a = numBits 0 a
    where
        numBits n 0 = n
        numBits n a = case a .&. 1 of
                          0 -> numBits (n + 1) (a `shiftR` 1)
                          otherwise -> n

zeroes :: Stream Integer
zeroes = streamRepeat 0

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0)
    (streamMap numZeroRightBits (streamFromSeed (+2) 2))

-- exercise 6

x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

scalarMul :: Integer -> Stream Integer -> Stream Integer
scalarMul n (Cons x xs) = Cons (n * x) $ scalarMul n xs

instance Num (Stream Integer) where
    fromInteger n = Cons n $ streamRepeat 0
    negate (Cons x xs) = Cons (-x) $ negate xs
    (+) (Cons a as) (Cons b bs) = Cons (a + b) $ as + bs
    (*) (Cons a as) bigB@(Cons b bs) =
            Cons (a * b) $ (scalarMul a bs) + (as * bigB)

instance Fractional (Stream Integer) where
    (/) (Cons a0 a') (Cons b0 b') =
        Cons (a0 `div` b0) $ scalarMul (1 `div` b0) (a' - q * b')
        where
            q :: Stream Integer
            q = Cons (a0 `div` b0) (scalarMul (1 `div` b0) (a' - q * b'))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- exercise 7

type Matrix = ((Integer, Integer), (Integer, Integer))

matrixRow :: Integer -> Matrix -> (Integer, Integer)
matrixRow n m
    | n == 0 = fst m
    | n == 1 = snd m
    | otherwise = error $ show n ++ " out of row num"

matrixCol :: Integer -> Matrix -> (Integer, Integer)
matrixCol n m
    | n == 0 = ((fst . fst) m, (fst . snd) m)
    | n == 1 = ((snd . fst) m, (snd . snd) m)
    | otherwise = error $ show n ++ " out of col num"

tupleMul :: (Integer, Integer) -> (Integer, Integer) -> Integer
tupleMul t1 t2 = fst t1 * fst t2 + snd t1 * snd t2

instance Num (Matrix) where
    (*) m1 m2 = ((tupleMul (matrixRow 0 m1) (matrixCol 0 m2),
                tupleMul (matrixRow 0 m1) (matrixCol 1 m2)),
                (tupleMul (matrixRow 1 m1) (matrixCol 0 m2),
                tupleMul (matrixRow 1 m1) (matrixCol 1 m2)))

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n =
        let f = ((1, 1), (1, 0)) :: Matrix
            fm = f^n
        in (snd . fst) fm
