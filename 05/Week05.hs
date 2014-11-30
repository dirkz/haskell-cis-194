import ExprT
import Parser

-- exercise 1

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

-- exercise 2

evalStr :: String -> Maybe Integer
evalStr expr = let tmp = parseExp Lit Add Mul expr in
  case tmp of
    Nothing -> Nothing
    Just e -> Just $ eval e

-- exercise 3

