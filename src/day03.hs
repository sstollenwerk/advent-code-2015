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
part1 = uniques . seen . map conv


keepEveryN :: [a] -> Int -> Int -> [a]
keepEveryN [] _ _ = []
keepEveryN (x:xs) 0 k = x:(keepEveryN xs (k-1) k)
keepEveryN (x:xs) n k = keepEveryN xs (n-1) k

seen :: [Complex Float] -> [Complex Float]
seen = ( scanl (+) 0 )

uniques :: Eq a => [a] -> Int
uniques = length . nub


part2 :: [Char] -> Int
part2 xs = uniques $ (seen (keepEveryN ys 0 2) ) ++ (seen (keepEveryN ys 1 2) )
    where ys = map conv xs

main :: IO ()
main = do 
          file <- readFile ("../data/03.txt")
          putStrLn ( "p1: " ++ (show $ part1 file ) )
          putStrLn ( "p2: " ++ (show $ part2 file ) )


