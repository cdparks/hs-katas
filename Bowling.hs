module Bowling where
import Debug.Trace
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
score ls = loop 0 frames where
    frames = toFrames ls
    (regular, bonus) = splitAt 10 frames
    loop 10 _ = 0
    loop n [] = 0
    loop n [Complete x y] = x + y
    loop n [Partial x]    = x
    loop n [Strike]       = 10
    loop n (x:xs)         = frameScore xs x + loop (succ n) xs

frameScore :: [Frame] -> Frame -> Int
frameScore _ (Partial a) = a
frameScore xs Strike     = 10 + getNextRolls 2 xs
frameScore xs (Complete a b) = current + next where
    current = a + b
    next | current == 10 = getNextRolls 1 xs
         | otherwise     = 0

scoreBonus :: [Frame] -> Frame -> Int
scoreBonus frames Strike = let x = getNextRolls 2 frames in trace (show x) x
scoreBonus frames (Complete a b)
    | a + b == 10 = getNextRolls 1 frames
    | otherwise   = 0
scoreBonus frames (Partial a) = 0

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

main = print $ runTests [ []
                        , [1,2,3,4]
                        , [7,3,8,1]
                        , [10,8,1]
                        , [1,2,3]
                        , [7,3]
                        , [10]
                        , [10,3]
                        , replicate 10 10
                        , replicate 11 10
                        , replicate 12 10
                        ] [0, 10, 27, 28, 6, 10, 10, 16, 270, 290, 300]
