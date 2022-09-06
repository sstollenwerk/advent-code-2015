module Day15 where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List.Split (splitOn)


-- import Boltzmann.Data.Common (partitions)
-- can't install - missing c packages

type Ingredient = Map String Int

asIngr :: String -> (String, Int)
asIngr s = (d, read amt)
    where
        (d:amt:[]) = words s


parseRow :: String -> Ingredient
parseRow s = Map.fromList $ map asIngr groups
    where 
        info = (splitOn ": " s) !!1 
        groups = splitOn ", " info

score1 :: Ingredient -> Int
score1 =  (foldr1 (*) ) . ( map (max 0) ) .  Map.elems . (Map.delete "calories" )

mix :: Ingredient -> Ingredient -> Ingredient
mix = Map.unionWith (+)


-- 4 ingedients = 100^4 =  100 billion?
-- nope: tetrahedral numbers - 'only' 176851
--partitions' :: Int -> Int -> [[Int]]
--partitions' 0 0 = [[]]
--partitions' 0 n = error "bad"
--partitions' k n =  [ i:p |    i <-  [0..k],  p <- (partitions (k-i) (n-1) ) ] 
-- results in stack overflow - not sure why

-- based on boltzmann-samplers-0.1.1.0  Boltzmann.Data.Common.html - partitions
-- modified so gets total that == k instead of <= k
partitions :: Int -> Int -> [[Int]]
partitions _ 0 = [[]]
partitions k 1 = [[k]]
partitions k n = do
  p <- [0 .. k]
  (p :) <$> partitions (k - p) (n - 1)

combine ::  [Ingredient] -> [Int] -> [Ingredient]
combine ingrs amts
    | (length ingrs) /= (length amts) = error "bad"
    | otherwise = [Map.map  (*amt) ingr |  (amt, ingr) <- zip  amts  ingrs ]

allPosses ::   Int -> [Ingredient]   -> [Ingredient]
allPosses totalTake ingrs  = map (foldl1 mix) groups
    where
        allParts = partitions totalTake (length ingrs)
        groups = map (combine ingrs) allParts


part1 :: String -> Int
part1 s = maximum $ map score1 $ allPosses 100 ingrs
    where
        ingrs = map parseRow (lines s)

part2 :: String -> Int
part2 s = 0