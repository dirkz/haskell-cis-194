-- | Main entry point to the application.
module Main where

import ExprT
import Calc
import Parser

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "CIS-194 Week 05"
    print $ eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20
    print $ evalStr "(2+3)*4" == Just 20
    print $ evalStr "2+3*4" == Just 14
    print $ evalStr "2+3*4+" == Nothing
    print $ (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT) == Mul (Add (Lit 2) (Lit 3)) (Lit 4)
