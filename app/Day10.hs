module Day10 where

import Data.List (group)

lookSay :: String -> String
lookSay = concat . (map lookGroup) . group

lookGroup :: String -> String
lookGroup s = (show (length s)) ++ [s !! 0]

part1 :: String -> Int
part1 = length . (!! 40) . (iterate lookSay)

part2 :: String -> Int
part2 s = length ((iterate lookSay s) !! 50)
