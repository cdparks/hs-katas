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
    loop (x:xs)         = (case x of
                            Complete a b -> let c = a + b in c + 
                                            (if c == 10
                                            then getNextRolls 1 xs
                                            else 0)
                            Partial a    -> a
                            Strike       -> 10 + getNextRolls 2 xs) + loop xs

getNextRolls :: Int -> [Frame] -> Int
getNextRolls _ []     = 0
getNextRolls 1 (x:xs) = case x of
                          Complete a b -> a
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
