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
    loop (x:y:ys) = undefined


