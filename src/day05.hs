module Main where

import Data.List (group, isInfixOf)

vowels = "aeiou"

c0 :: String -> Bool
c0 = ( >= 3) .  length . filter (flip elem vowels)

c1 :: String -> Bool
c1  = (any (>= 2) ) . (map  length ) . group

c2 :: String -> Bool
c2 s = not . or $ zipWith isInfixOf  ["ab", "cd", "pq", "xy"] (repeat s)



flist :: [(a -> b)] -> a -> [b]
flist [] _ = []
flist (x:xs) n = (x n) : (flist xs n)


nice :: String -> Bool
nice = and . flist [c0, c1, c2]

part1 :: [String] -> Int
part1 = length . filter nice

main :: IO ()
main = do 
          file <- readFile ("../data/05.txt")
          let parts = lines file
          putStrLn ( "p1: " ++ (show $ part1 parts ) )
          --putStrLn ( "p2: " ++ (show $ part2 parts ) )



