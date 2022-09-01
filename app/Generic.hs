module Generic where

import Data.List (tails, elemIndex)

flist :: [(a -> b)] -> a -> [b]
flist [] _ = []
flist (x:xs) n = (x n) : (flist xs n)

partsSizeN :: Int -> [a] -> [[a]]
partsSizeN n = filter ((== n) . length) . map (take n) . tails

findSublist :: Eq a => [a] -> [a] -> Maybe Int
findSublist part total = elemIndex part (partsSizeN (length part) total)