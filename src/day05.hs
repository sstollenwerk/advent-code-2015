module Main where

import Data.List (group, isInfixOf)

import Data.Map (Map)
import qualified Data.Map as Map



vowels = "aeiou"

c0 :: String -> Bool
c0 = ( >= 3) .  length . filter (flip elem vowels)

c1 :: Eq a => [a] -> Bool
c1  = (any (>= 2) ) . (map  length ) . group

c2 :: String -> Bool
c2 s = not . or $ zipWith isInfixOf  ["ab", "cd", "pq", "xy"] (repeat s)



flist :: [(a -> b)] -> a -> [b]
flist [] _ = []
flist (x:xs) n = (x n) : (flist xs n)


nice1 :: String -> Bool
nice1 = and . flist [c0, c1, c2]

part1 :: [String] -> Int
part1 = length . filter nice1

--c3 :: Ord a => [a] -> Bool
--c3 = (any (>= 2) )  . (Map.foldr (:) [] ) .  counter . pairs

c3 :: Eq a => [a] -> Bool
c3 [] = False 
c3 xs = (isInfixOf ( take 2 xs   )  (drop 2 xs)  ) || c3 (tail xs )

c4 :: Eq a => [a] -> Bool
c4 xs = or $ zipWith (==) xs (drop 2 xs)

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

counter :: Ord a => [a] -> Map a Int
counter [] = Map.empty
counter (k:kx) =  Map.insertWith (+) k 1 (counter kx)



nice2 :: String -> Bool
nice2 = and . flist [c3, c4]


part2 :: [String] -> Int
part2 = length . filter nice2


main :: IO ()
main = do 
          file <- readFile ("../data/05.txt")
          let parts = lines file
          putStrLn ( "p1: " ++ (show $ part1 parts ) )
          putStrLn ( "p2: " ++ (show $ part2 parts ) )



