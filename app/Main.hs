module Main where

import Day09 (part1, part2, makeGraph, requiredCities)




main :: IO ()
main = do 
          file <- readFile (directory ++ "09.txt")
          let parts = lines file
          putStrLn ( "p1: " ++ (show $ part1 parts ) )
          putStrLn ( "p2: " ++ (show $ part2 parts ) )
          print $ makeGraph parts
          print $ requiredCities parts





directory = "E:/important/programming/advent-code-2015/data/"
