module Day17 where

import Data.List (subsequences)

canFill' :: Int -> [Int] -> Int
canFill' 0 _ = 1
canFill' n [] = 0


-- O(2^n) - n~= 20, small enough
canFillBrute :: Int -> [Int] -> Int
canFillBrute n = length . filter ((n==) . sum ) . subsequences 


part1 :: Int ->  String -> Int
part1 n = (canFillBrute n) . (map read ) . lines

part2 :: String -> Int 
part2 s = 0