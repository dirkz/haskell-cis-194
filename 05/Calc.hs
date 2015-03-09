{-# LANGUAGE FlexibleInstances #-} -- exercise 5

module Calc where

import qualified Data.Map.Strict as M
import qualified ExprT as E -- exercise 1
import Parser -- exercise 2
import qualified StackVM as S -- exercise 5

-- exercise 1

eval :: E.ExprT -> Integer
eval (E.Lit a) = a
eval (E.Add e1 e2) = (eval e1) + (eval e2)
eval (E.Mul e1 e2) = (eval e1) * (eval e2)

-- exercise 2

evalStr :: String -> Maybe Integer
evalStr s = let expr = parseExp E.Lit E.Add E.Mul s
            in case expr of
                Just e -> Just $ eval e
                otherwise -> Nothing

-- exercise 3

class Expr e where
    lit :: Integer -> e
    mul :: e -> e -> e
    add :: e -> e -> e

instance Expr E.ExprT where
    lit = E.Lit
    mul = E.Mul
    add = E.Add

-- exercise 4

instance Expr Integer where
    lit = id
    mul = (*)
    add = (+)

instance Expr Bool where
    lit i
        | i <= 0 = True
        | otherwise = False
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min

mod7 :: Integer -> Mod7
mod7 n = Mod7 $ mod n 7

instance Expr Mod7 where
    lit i
        | i > 6 || i < 0 = error $ (show i) ++ " is out of range: 0..6"
        | otherwise = Mod7 i
    add (Mod7 m1) (Mod7 m2) = mod7 $ m1 + m2
    mul (Mod7 m1) (Mod7 m2) = mod7 $ m1 * m2

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- exercise 5

instance Expr S.Program where
    lit i = [S.PushI i]
    mul a b = a ++ b ++ [S.Mul]
    add a b = a ++ b ++ [S.Add]

testStack = testExp :: Maybe S.Program

compile :: String -> Maybe S.Program
compile s = parseExp lit add mul s

-- exercise 6

class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
    lit = Lit
    mul = Mul
    add = Add

instance HasVars VarExprT where
    var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

type LookupFn = (M.Map String Integer -> Maybe Integer)
type OpFn = Integer -> Integer -> Integer

op :: OpFn -> LookupFn -> LookupFn -> M.Map String Integer -> Maybe Integer
op op f1 f2 m =
    let a = f1 m
        b = f2 m
    in case (a,b) of
        (Just n1, Just n2) -> Just $ op n1 n2
        otherwise -> Nothing

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit = \x m -> Just x
    mul = op (*)
    add = op (+)

withVars :: [(String, Integer)]
    -> (M.Map String Integer -> Maybe Integer)
    -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
