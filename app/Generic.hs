module Generic where

import Data.List (tails, elemIndices)

flist :: [(a -> b)] -> a -> [b]
flist [] _ = []
flist (x:xs) n = (x n) : (flist xs n)

partsSizeN :: Int -> [a] -> [[a]]
partsSizeN n = filter ((== n) . length) . map (take n) . tails

findSublists :: Eq a => [a] -> [a] -> [Int]
findSublists part total = elemIndices part (partsSizeN (length part) total)

pairwise :: [a] -> [(a,a)]
pairwise xs = zip xs (tail xs)