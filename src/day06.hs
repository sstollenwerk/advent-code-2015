module Main where

import Data.Complex
import Data.List.Split (splitOn)
import Data.List( foldl')

import Data.Map (Map)
import qualified Data.Map as Map


import Data.Set (Set)
import qualified Data.Set as Set


data Command = Off | On | Toggle deriving (Eq,Ord,Enum,Show)



-- data Position = Complex Int deriving (Eq, Show)

type Position = (Int, Int)

type Lights = Map  Position Int

data Instruction = Instruction {
    c :: Command,
    a :: Position,
    b :: Position
    } deriving (Show)

data Inst = Inst {
    d :: Command,
    e :: Position
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

com Off = (max 0) . pred
com On = (+1)
com Toggle = (+2)

allCommands :: [Instruction] -> [Inst]
allCommands =  concat . (map asComm )

asComm :: Instruction -> [Inst]
asComm (Instruction k a b) = [(Inst k (x , y) ) | x <- ([fst a .. fst b] ) , y <- [snd  a .. snd  b] ]

switch2 :: Lights -> Inst -> Lights
switch2 m (Inst k a) = Map.insert a (com k v) m
    where v = Map.findWithDefault 0 a m




part1 :: [Instruction] -> Int
part1 = length .  (foldl' switch (Set.empty)  )

part2 :: [Instruction] -> Int
part2 xs = sum $ Map.elems  $  (foldl' switch2 (Map.empty)   (allCommands xs) )


main :: IO ()
main = do 
          file <- readFile ("../data/06.txt")
          let parts = map parse $ lines file
          --mapM_  print parts
          print $ vals (0,0) (10,0)
          putStrLn ( "p1: " ++ (show $ part1 [(Instruction Toggle (0,0) (999,0) )] ) ) 
          putStrLn ( "p1: " ++ (show $ part1 parts ) )
          putStrLn ( "p2: " ++ (show $ part2 parts ) )