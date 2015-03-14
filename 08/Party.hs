module Party where

import Data.Monoid
import Data.Tree
import Data.List
import Employee

-- exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) (fun + empFun e)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL ems1 fun1) (GL ems2 fun2) = GL (ems1 ++ ems2) (fun1 + fun2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 =
    case (compare gl1 gl2) of
        GT -> gl1
        otherwise -> gl2

-- exercise 2

treeList :: Tree a -> [a]
treeList (Node a []) = [a]
treeList (Node a xs) = a : concat (map treeList xs)

treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold f acc t = foldl' f acc $ treeList t

-- exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel bob = foldl' stepper (GL [] 0, GL [] 0)
    where stepper acc (glWithSubBoss@(GL _ funWithoutBob), glWithoutSubBoss@(GL _ funWithBob)) =
            (glWithSubBoss, glWithSubBoss)
