module Bowling where

data Frame = Complete Int Int
           | Partial Int
           | Strike
             deriving Show

data Mode = Normal
          | Bonus
            deriving Show

toFrames :: [Int] -> [Frame]
toFrames []     = []
toFrames [10]   = [Strike]
toFrames [x]    = [Partial x]
toFrames (x:y:xs)
    | x == 10   = Strike : toFrames (y:xs)
    | otherwise = Complete x y : toFrames xs

score :: [Int] -> Int
score ls = loop $ toFrames ls where
    loop [] = 0
    loop [Complete x y] = x + y
    loop [Partial x]    = x
    loop [Strike]       = 10
    loop (x:xs)         = frameScore xs x + loop xs

frameScore :: [Frame] -> Frame -> Int
frameScore _ (Partial a) = a
frameScore xs Strike     = 10 + getNextRolls 2 xs
frameScore xs (Complete a b) = current + next where
    current = a + b
    next | current == 10 = getNextRolls 1 xs
         | otherwise     = 0

getNextRolls :: Int -> [Frame] -> Int
getNextRolls _ []     = 0
getNextRolls 1 (x:xs) = case x of
    Complete a _ -> a
    Partial a    -> a
    Strike       -> 10
getNextRolls 2 (Strike:x:xs) = 10 + getNextRolls 1 (x:xs)
getNextRolls 2 (x:xs) = case x of
    Complete a b -> a + b
    Partial  a   -> a
    Strike       -> 10
getNextRolls _ _ = undefined

runTests :: [[Int]] -> [Int] -> Bool
runTests tests solns = map score tests == solns

main = print $ runTests [[], [1,2,3,4], [7,3,8,1], [10,8,1], [1,2,3], [7,3], [10], [10,3]] [0, 10, 27, 28, 6, 10, 10, 16]
