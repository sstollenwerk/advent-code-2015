module Main where

import Day20 (part1, part2, part1Outside)




main :: IO ()
main = do 
          file <- readFile (directory ++ "20.txt")
          --let parts = lines file
          oeis <- readFile (directory ++ "b002093.txt")
          --putStrLn ( "p1: " ++ (show $ part1 file ) )
          putStrLn ( "p1: " ++ (show $ part1Outside file oeis ) )

          putStrLn ( "p2: " ++ (show $ part2 file ) )





directory = "E:/important/programming/advent-code-2015/data/"
