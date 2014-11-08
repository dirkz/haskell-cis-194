{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Data.List (isInfixOf)
import Log

-- exercise 1

parseMessageComps :: [String] -> LogMessage
parseMessageComps ("E":errno:ts:xs) = LogMessage (Error (read errno)) (read ts) (unwords xs)
parseMessageComps ("I":ts:xs) = LogMessage Info (read ts) (unwords xs)
parseMessageComps ("W":ts:xs) = LogMessage Warning (read ts) (unwords xs)
parseMessageComps xs = Unknown (unwords xs)

parseMessage :: String -> LogMessage
parseMessage = parseMessageComps . words

parseLines :: [String] -> [LogMessage]
parseLines [] = []
parseLines (x:xs) = parseMessage x : parseLines xs

parse :: String -> [LogMessage]
parse = parseLines . lines

-- exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert l Leaf = Node Leaf l Leaf
insert lm@(LogMessage _ tsNew _) (Node left lmExisting@(LogMessage _ tsExisting _) right) =
  if tsNew < tsExisting then Node (insert lm left) lmExisting right
  else Node left lmExisting (insert lm right)
insert lm (Node left (Unknown _) right) = (Node left lm right)

-- exercise 3

build :: [LogMessage] -> MessageTree
build [] = Leaf
build [x] = insert x Leaf
build (x:xs) = insert x (build xs)

-- exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lm right) = (inOrder left) ++ [lm] ++ (inOrder right)

-- exercise 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ((LogMessage (Error crit) _ msg):xs) =
  if crit >= 50 then msg : whatWentWrong xs
  else whatWentWrong xs
whatWentWrong (_:xs) = whatWentWrong xs

-- exercise 6

-- filtering only messages that contain "mustard" doesn't seem to be the solution

isImportantMessage :: String -> Bool
isImportantMessage msg = if isInfixOf "mustard" msg then True else False

isImportantLog :: LogMessage -> Bool
isImportantLog lm = isImportantMessage $ extractMessage lm

extractMessage :: LogMessage -> String
extractMessage = show

allMessages :: [LogMessage] -> [String]
allMessages [] = []
allMessages (x:xs) = if (isImportantLog x) then extractMessage x : allMessages xs
                     else allMessages xs
