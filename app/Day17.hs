module Day17 where

import Data.List (subsequences,sort, group )


-- O(2^n) - n~= 20, small enough
canFillBrute :: Int -> [Int] -> Int
canFillBrute n = length . allWays n 

allWays :: Int -> [Int] -> [[Int]]
allWays n = filter ((n==) . sum ) . subsequences 

smallestCanFill :: Int -> [Int] -> Int
smallestCanFill n xs = length . head .  group . sort . (map length) $ allWays n xs



part1 :: Int ->  String -> Int
part1 n = (canFillBrute n) . (map read ) . lines

part2 :: Int ->  String -> Int
part2 n = (smallestCanFill n) . (map read ) . lines