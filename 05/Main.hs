-- | Main entry point to the application.
module Main where

import ExprT as E
import Calc
import Parser
import StackVM as VM

-- exercise 4

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 5"

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "CIS-194 Week 05"
    -- exercise 1
    print $ eval (E.Mul (E.Add (Lit 2) (Lit 3)) (Lit 4)) == 20
    -- exercise 2
    print $ evalStr "(2+3)*4" == Just 20
    print $ evalStr "2+3*4" == Just 14
    print $ evalStr "2+3*4+" == Nothing
    -- exercise 4
    print $ (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT) == E.Mul (E.Add (Lit 2) (Lit 3)) (Lit 4)
    print (testExp :: Maybe Integer)
    print (testExp :: Maybe Bool)
    print (testExp :: Maybe MinMax)
    print (testExp :: Maybe Mod7)
