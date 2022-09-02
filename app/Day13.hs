
module Day13 where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Tuple (swap)

import Data.Maybe (fromJust)

import Data.List (sort, permutations )


import Generic (pairwise)

type Name = String

type Amt = Int

type Pair = (Name, Name)

type Delta = (Pair, Amt)

type Happiness = Map Pair Amt

asDel :: String -> Int
asDel "gain" = 1
asDel "lose" = (-1)

parseRow :: String -> Delta
parseRow s = ( ( (xs !! 0), (last xs) ), (  read (xs!!3) * (asDel (xs!!2)) )  )
  where
    xs = words (init s)
-- nameA 0
-- +- 2
-- amt -- 3
-- nameB = last


happy :: Happiness -> [Name] -> Amt
happy h xs = sum  . (map fromJust) $ map (`Map.lookup` h) groups
    where
        ys = (last xs): xs
        p1 = pairwise ys
        groups = p1 ++ (map swap p1)

names :: Happiness -> [Name]
names = sort . Set.toList  .  (Set.map  fst) . Map.keysSet 

part1 :: String -> Amt
part1 words = maximum $ map  (happy h) (permutations (names h) )
    where h = Map.fromList  $ map parseRow $ lines words



part2 :: String -> Amt
part2 w = 0
