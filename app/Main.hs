module Main where

import Day17 (part1, part2)




main :: IO ()
main = do 
          file <- readFile (directory ++ "17.txt")
          --let parts = lines file
          putStrLn ( "p1: " ++ (show $ part1 25 "20\n15\n10\n5\n5" ) )
          putStrLn ( "p1: " ++ (show $ part1 150 file ) )
          putStrLn ( "p2: " ++ (show $ part2 file ) )





directory = "E:/important/programming/advent-code-2015/data/"
