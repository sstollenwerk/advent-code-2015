module Day20 where

import Data.List (sort)

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


presents2BruteForce = (11*) . sum . (take 50) . orderedFactors

orderedFactors = sort . factors
-- write more efficient implementation

--part1 :: String -> Int
--part1 s = head $ filter ( (>=(read s) ) . presentsBruteForce) [1..]
part1 s = 0

part1Outside :: String -> String -> Int
part1Outside s oeis = head $ filter ( (>=(read s) ) . presentsBruteForce) posses
    where posses = map (read . (!!1) . words ) $ lines  oeis

part2 s = head $ filter ( (>=(read s) ) . presents2BruteForce) [1..]
