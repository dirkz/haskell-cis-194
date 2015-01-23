module Calc where

import ExprT
import Parser

-- exercise 1

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- exercise 2

evalStr :: String -> Maybe Integer
evalStr s = case parse s of
                Just exp -> Just $ eval exp
                Nothing -> Nothing
    where parse = parseExp Lit Add Mul

-- exercise 3

class Expr e where
    lit :: Integer -> e
    mul :: e -> e -> e
    add :: e -> e -> e

instance Expr ExprT where
    lit = Lit
    mul = Mul
    add = Add

instance Expr Integer where
    lit = id
    mul a b = a * b
    add a b = a + b

instance Expr Bool where
    lit a
        | a <= 0 = False
        | otherwise = True
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax $ max a b
    mul (MinMax a) (MinMax b) = MinMax $ min a b

instance Expr Mod7 where
    lit a = Mod7 $ mod a 7
    add (Mod7 a) (Mod7 b) = Mod7 $ mod a 7 + mod b 7
    mul (Mod7 a) (Mod7 b) = Mod7 $ mod a 7 * mod b 7