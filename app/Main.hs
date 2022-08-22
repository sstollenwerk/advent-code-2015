module Main where

import Day07 (part1, parse, part2)


main :: IO ()
main = do 
          file <- readFile (directory ++ "07.txt")
          let parts = map parse $ lines file
          putStrLn ( "p1: " ++ (show $ part1 parts "a" ) )
          putStrLn ( "p2: " ++ (show $ part2 parts "a" ) )




directory = "E:/important/programming/advent-code-2015/data/"
