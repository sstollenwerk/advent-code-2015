module Day14 where

import Data.List (transpose, group, sort, maximum )
import Data.Function (on)

import Debug.Trace (traceShowId)


type Movement = (Int, Int, Int)

type Reindeer = (String, Movement )

parseRow :: String -> Reindeer
parseRow s = ( xs!!0,  (read (xs!! 3), read (xs!! 6), read (xs!! 13)  )  )
  where
    xs = words s

position :: Reindeer -> [Int]
position (name,  (speed, take, rest)  ) = scanl1 (+) $ cycle (  (replicate take speed ) ++ (replicate rest 0)  )

movement :: Reindeer -> Int -> Int
movement r time
    | time <= take = speed*time
    | time <= take+rest = speed*take
    | otherwise = reps*speed*take  + movement r rem

    where 
        (name,  (speed, take, rest)  ) = r 
        (reps, rem) = divMod time (take+rest)

argMaximums  :: Ord a => [a] -> [Int]
argMaximums xs = filter ( (best==) . (xs!!)   ) [0..(length xs) - 1]
    where
        best = maximum xs


mostCommonAppears :: Ord a => [a] -> Int
mostCommonAppears = maximum . (map length) . group . sort

part1 :: Int -> String -> Int
part1 time = maximum  . map (`movement` time)  . map (parseRow) . lines

part2 :: Int -> String -> Int
part2 time = mostCommonAppears . best  . eachRow
    where
        eachRow = (take time ) . transpose . ( map (position . parseRow) ) . lines
        best = concat . (map (argMaximums))