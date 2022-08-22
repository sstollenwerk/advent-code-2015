module Main where

import Data.List (elemIndex)

import Data.Maybe (fromJust)

conv :: Char -> Int
conv '(' = 1
conv ')' = -1
conv _ = error "invalid input"

part1 :: [Char] -> Int
part1 = sum . map conv


part2 :: [Char] -> Int
part2 = (1+) . fromJust . ( elemIndex  (-1) ) . ( scanl1 (+) ) .  map conv 




main :: IO ()
main = do 
          file <- readFile ("../data/01.txt")
          putStrLn ( "p1: " ++ (show $ part1 file ) )
          putStrLn ( "p2: " ++ (show $ part2 file ) )


