module Main where

import Day21 (part1, part2)




main :: IO ()
main = do 
          file <- readFile (directory ++ "21.txt")
          --let parts = lines file
          putStrLn ( "p1: " ++ (show $ part1 file ) )

          putStrLn ( "p2: " ++ (show $ part2 file ) )





directory = "E:/important/programming/advent-code-2015/data/"
