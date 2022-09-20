module Day21 where

import Data.List (subsequences )
import Data.Maybe (isJust )

import Control.Monad (liftM3 )

import Debug.Trace (traceShowId)


data Equipment = Equipment { 
  name :: String
, cost :: Int  
, damage :: Int  
, armour :: Int  
} deriving (Show,Eq)   

data Character = Character {
    hp :: Int
,   damage_ :: Int  
,   armour_ :: Int

} deriving (Show,Eq)   

toCharacter :: String -> Character
toCharacter s = Character (xs!!0) (xs!!1) (xs!!2)
    where xs = map (read . last . words) $ lines s

combine :: Equipment -> Equipment -> Equipment
combine (Equipment n1 c1 d1 a1) (Equipment n2 c2 d2 a2) = Equipment (n1++ ',':n2) (c1+c2) (d1+d2) (a1+a2)

weapons = [ (Equipment "Dagger" 8 4 0) , (Equipment "Shortsword" 10 5 0),  (Equipment "Warhammer" 25 6 0),  (Equipment "Longsword" 40 7 0),  (Equipment "Greataxe" 74 8 0)]
armours = [ (Equipment "Leather" 13 0 1) , (Equipment "Chainmail" 31 0 2),  (Equipment "Splintmail" 53 0 3),  (Equipment "Bandedmail" 75 0 4),  (Equipment "Platemail" 102 0 5)  ]
rings = [ (Equipment "Damage +1" 25 1 0) , (Equipment "Damage +2" 50 2 0),  (Equipment "Damage +3" 100 3 0),  (Equipment "Defense +1" 20 0 1),  (Equipment "Defense +2" 40 0 2), (Equipment "Defense +3" 80 0 3) ]

-- brute force = 5 * 6 *  (6c0 + 6c1 + 6c2) = 5*6*22 = 660 = small enough.


combinations k ns = filter ((k==).length) $ subsequences ns
-- from https://stackoverflow.com/a/52605612


w = (combinations 1 weapons)
a = (combinations 0 armours) ++ (combinations 1 armours)
r = (combinations 0 rings) ++ (combinations 1 rings) ++ (combinations 2 rings) 
posses = [ (foldl1 combine)    (w_++a_++r_) |  w_ <- w, a_ <- a, r_ <- r]

wins :: Character -> Character -> Bool
wins (Character hp1 d1 arm1) (Character hp2 d2 arm2)
    | hp1 <= 0 = False
    | otherwise = not $ wins (Character (hp2 - (max 1 (d1-arm2)  ) ) d2 arm2) (Character hp1 d1 arm1)

equipWins :: Int -> Character -> Equipment -> Bool
equipWins hp boss (Equipment _ _ dam arm) = wins (Character hp dam arm ) boss


part1 :: String -> Int
part1 s = minimum $ map cost $ filter (equipWins 100 (toCharacter s) ) posses

part2 :: String -> Int
part2 s = maximum $ map cost  $ filter (not . ( equipWins 100 (toCharacter s) ) ) posses