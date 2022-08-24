module Main where

import Day08 (part1, part2)




main :: IO ()
main = do 
          file <- readFile (directory ++ "08.txt")
          let parts = lines file
          putStrLn ( "p1: " ++ (show $ part1 parts ) )
          putStrLn ( "p2: " ++ (show $ part2 parts ) )





directory = "E:/important/programming/advent-code-2015/data/"
