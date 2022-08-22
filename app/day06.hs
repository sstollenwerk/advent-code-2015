module Main where

import Data.Complex
import Data.List.Split (splitOn)
import Data.List( foldl')

import Data.Map (Map)
import qualified Data.Map as Map


import Data.Set (Set)
import qualified Data.Set as Set


data Command = Off | On | Toggle deriving (Eq,Ord,Enum,Show)

type Position = (Int, Int)

type Lights = Map  Position Int

data Instruction = Instruction {
    c :: Command,
    a :: Position,
    b :: Position
    } deriving (Show)


asPos :: String -> Position
asPos s =  (xs!!0 , xs!! 1)
    where xs = map read $ splitOn "," s

parse :: String -> Instruction
parse s
    |parts!!1 == "on" = (Instruction On a b)
    |parts!!1 == "off" = (Instruction Off a b)
    |parts!!0 == "toggle" = (Instruction Toggle a b)
    where parts = words s
          l = length parts
          b = asPos $ parts!!(l-1)
          a = asPos $ parts!!(l-3)

vals a b = Set.fromList  [(x , y) | x <- ([fst a .. fst b] ) , y <- [snd  a .. snd  b] ]

switch :: Set Position -> Instruction -> Set Position
switch p (Instruction i a b) = (group i) p (vals a b)


group Off = Set.difference
group On = Set.union
group Toggle = symmetricDifference

symmetricDifference :: Ord a => Set a -> Set a -> Set a
symmetricDifference a b = Set.union (Set.difference  a b ) (Set.difference  b a )


com :: Command -> Int
com Off = (-1)
com On = 1
com Toggle = 2


switch2 :: Lights -> Instruction -> Lights
switch2 m (Instruction k a b) = Map.map (max 0) res
    where res = Map.unionWith (+) m $ Map.fromList [ (x, com k )  | x <- Set.toList (vals a b)  ]


part1 :: [Instruction] -> Int
part1 = length .  (foldl' switch (Set.empty)  )

part2 :: [Instruction] -> Int
part2 xs = sum $ Map.elems  $  foldl' switch2 (Map.empty)   xs


main :: IO ()
main = do 
          file <- readFile ("../data/06.txt")
          let parts = map parse $ lines file
          putStrLn ( "p1: " ++ (show $ part1 parts ) )
          putStrLn ( "p2: " ++ (show $ part2 parts ) )