module Main where

import Data.Complex
import Data.List.Split (splitOn)


import Data.Set (Set)
import qualified Data.Set as Set


data Command = Off | On | Toggle deriving (Eq,Ord,Enum,Show)



-- data Position = Complex Int deriving (Eq, Show)

type Position = (Int, Int)

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
switch p (Instruction Off a b) = Set.difference p (vals a b)
switch p (Instruction On a b) = Set.union  (vals a b) p
switch p (Instruction Toggle a b) = Set.union (Set.difference  p xs ) (Set.difference  xs p )
    where xs = (vals a b)

part1 :: [Instruction] -> Int
part1 = length .  (foldl switch (Set.empty)  )

main :: IO ()
main = do 
          file <- readFile ("../data/06.txt")
          let parts = map parse $ lines file
          --mapM_  print parts
          print $ vals (0,0) (10,0)
          putStrLn ( "p1: " ++ (show $ part1 [(Instruction Toggle (0,0) (999,0) )] ) ) 
          putStrLn ( "p1: " ++ (show $ part1 parts ) )
          --putStrLn ( "p2: " ++ (show $ part2 parts ) )