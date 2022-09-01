module Day12 where

import Generic (partsSizeN)

import Data.Function (on)
import Data.List (elemIndex, elemIndices, findIndex, groupBy, sort)

import Data.Maybe (fromJust, fromMaybe, isNothing)

import Debug.Trace (traceShowId)

isNumeric :: Char -> Bool
isNumeric = flip elem ('-' : ['0' .. '9'])

groupNum :: Char -> Char -> Bool
groupNum = (==) `on` isNumeric

toNums :: String -> [Int]
toNums = map read . filter (isNumeric . head) . (groupBy groupNum)

part1 :: String -> Int
part1 = sum . toNums

toSearch = ":\"red\""

conv :: Char -> Int
conv '{' = 1
conv '}' = -1
conv _ = 0

findMatch :: [Char] -> Int
findMatch = fromJust . elemIndex (-1) . scanl1 (+) . map conv

removeSection :: String -> Int -> String
removeSection xs n = take start xs ++ drop end xs
  where
    start = last $ elemIndices '{' (take n xs)
    end = 2 + start + findMatch (drop (start + 1) xs)

findSublist :: Eq a => [a] -> [a] -> Maybe Int
findSublist part total = elemIndex part (partsSizeN (length part) total)

part2 :: String -> Int
part2 xs
  | isNothing red = part1 xs
  | otherwise = part2 $ removeSection xs (fromJust red)
  where
    red = findSublist toSearch xs
