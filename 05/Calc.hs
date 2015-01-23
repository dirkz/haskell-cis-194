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
