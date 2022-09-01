
module Day12 where


import Generic (partsSizeN)


import Data.List (groupBy,elemIndices, sort)
import Data.Function (on)

isNumeric :: Char -> Bool
isNumeric = flip elem ('-':['0'..'9'])

groupNum :: Char -> Char -> Bool
groupNum = (==) `on` isNumeric

toNums :: String -> [Int]
toNums = (map read) . (filter (isNumeric . head  )) . (groupBy groupNum )

part1 :: String -> Int
part1 = sum . toNums

toSearch = ":\"red\""


part2 xs
    where
        reds = elemIndices toSearch $ partsSizeN (length red) xs
        starts = elemIndices '{' xs
        ends = elemIndices '}' xs
        parts = sort $ [(i,1) | i <- starts ] ++ [(i,-1) | i <- ends ]
        (indices, amts) = unzip parts
        totals = foldl (+) amts
        places = (map fst) . filter ( (<=2) . snd) $ (zip indices totals)