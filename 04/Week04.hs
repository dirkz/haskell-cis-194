-- exercise 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

-- exercise 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

treeHeight :: Tree a -> Integer
treeHeight Leaf = 0
treeHeight (Node h _ _ _) = h

insertTree :: a -> Tree a -> Tree a
insertTree a Leaf = Node 0 Leaf a Leaf
insertTree a (Node h left c right) =
  if hLeft < hRight
  then (Node (hRight+1) (insertTree a left) c right)
  else (Node (hLeft+1) left c (insertTree a right))
  where hLeft = treeHeight left
        hRight = treeHeight right

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

-- exercise 3

xor :: [Bool] -> Bool
xor = odd . foldr (\x a -> if x then (a+1) else a) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x a -> (f x) : a) []

-- exercise 4
-- this is buggy, see filterOut

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

allCarts :: [(Integer, Integer)]
allCarts = cartProd [1,2..] [1,2..]

-- for a given natural number n, give all numbers of the form
-- i + j + 2 * i * j <= n, i > 1, i <= j
-- this is buggy, according to https://en.wikipedia.org/wiki/Sieve_of_Sundaram
-- it should also contain e.g. 27, but it doesn't
filterOut n =
  takeWhile (<=n) .
  map (\(i, j) -> i + j + 2 * i * j) .
  filter (\(i, j) -> i <= j) $ allCarts

-- for a given natural number n, is there no i and a j so that
-- i > 1, i <= j and j + j + 2 * i * j = n?
ijNo :: Integer -> Bool
ijNo n = let r = takeWhile (==n) .
                 dropWhile (<n) $ filterOut n
         in null r

filtered :: Integer -> [Integer]
filtered n = filter ijNo [1..n]

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (\n -> 2 * n + 1) . filtered
