module Main where

import Data.Complex
import Data.List (nub)


conv :: Char -> Complex Float
conv '^' = (0 :+ 1)
conv 'v' = (-1) * ( conv '^' )
conv '>' = (1 :+ 0)
conv '<' = (-1) *  ( conv '>' )
conv _ = error "invalid input"

part1 :: [Char] -> Int
part1 = length . nub . ( scanl (+) 0 ) . map conv




main :: IO ()
main = do 
          file <- readFile ("../data/03.txt")
          putStrLn ( "p1: " ++ (show $ part1 file ) )
          --putStrLn ( "p2: " ++ (show $ part2 file ) )


