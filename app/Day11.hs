module Day11 where

import Generic (flist)

import Data.List ((\\), elemIndex, group, tails)

import Data.Char (ord)

import Data.Maybe (fromJust)

ineleg = "iol"

letters = (' ' : ['a' .. 'z']) \\ ineleg

size' = toInteger $ length letters

incr :: Integer -> Char -> Integer
incr a c = (a * size') + ( toInteger $fromJust (elemIndex c letters) )

asNum :: [Char] -> Integer
asNum = foldl incr 0

fromNum :: Integer -> [Char]
fromNum 0 = []
fromNum n = fromNum (div n size') ++ [letters !! (fromIntegral $ mod n size')]

succ' :: String -> String
succ' = fromNum . succ . asNum

partsSizeN :: Int -> [a] -> [[a]]
partsSizeN n = filter ((== n) . length) . map (take n) . tails

c0' :: Enum a => Eq a => [a] -> Bool
c0' xs = xs == [xs !! 0 .. xs !! (length xs - 1)]

c0 :: Enum a => Eq a => [a] -> Bool
c0 xs = any c0' (partsSizeN 3 xs)

c1 :: String -> Bool
c1 xs = not $ any (`elem` xs) (' ' : ineleg)

c2 :: Eq a => [a] -> Bool
c2 = (>= 2) . length . filter ((>= 2) . length) . group

valid1 = and . flist [c0, c1, c2]

part1 :: String -> String
part1 = until valid1 succ'

part2 :: String -> String
part2 = part1 . succ' . part1
