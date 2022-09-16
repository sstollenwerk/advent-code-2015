module Day20 where

import Data.List (sort)

import           Data.Map.Lazy        (Map)
import qualified Data.Map.Lazy as Map


presentsBruteForce :: Int -> Int
presentsBruteForce = (10*) . sigma
-- solution must be in A002093		Highly abundant numbers: numbers k such that sigma(k) > sigma(m) for all m < k.
-- looked up how to download and parse https://oeis.org/A002093/b002093.txt in haskell - presentsBruteForce completed before could download packages and implement
-- maybe easier to download the oeis file manually?

-- use http-conduit and Data.ByteString.Lazy.Char8.unpack
-- https://stackoverflow.com/questions/9986404/how-can-i-do-an-https-request-in-haskell 
-- https://stackoverflow.com/questions/37571939/data-bytestring-lazy-internal-bytestring-to-string
-- 




factors :: Int -> [Int]
factors n = go 1
    where
        go :: Int -> [Int]
        go k
            |k^2 > n = []
            |k^2 == n = [k]
            |otherwise = poss ++ (go (k+1))
            where 
                poss = if n `mod` k == 0 then [k, n `div` k ] else []

sigma :: Int -> Int
sigma = sum . factors


--presents2BruteForce = (11*) . sum . (take 50) . orderedFactors
-- nope, that's not right

--orderedFactors = sort . factors


elf :: Int -> Map Int Int
elf n = Map.fromList [ (i, n*11) | i<- houses  ]
    where 
        houses = map (*n) [1..50]

part1 :: String -> Int
part1 s = head $ filter ( (>=(read s) ) . presentsBruteForce) [1..]

part1Outside :: String -> String -> Int
part1Outside s oeis = head $ filter ( (>=(read s) ) . presentsBruteForce) posses
    where posses = map (read . (!!1) . words ) $ lines  oeis

elvesUpTo n = Map.unionsWith (+) ( map elf [1..n]  )

part2 s = fst $ Map.findMin $ Map.filter (>= n ) allElves
    where 
        n = read s
        allElves = elvesUpTo (div n 10)
        -- elf (div n 10) will deliver more than n presents to a house so can stop by this point
        -- is it guaranteed that first house seen is smallest? No idea. Not assuming so.
