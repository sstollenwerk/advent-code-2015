module Day16 where

import           Data.List.Split (splitOn, chunksOf )
import           Data.List (sort )

import           Data.Map        (Map)
import qualified Data.Map        as Map

import           Data.Set        (Set)
import qualified Data.Set        as Set




-- would use dataclass in python - not sure how to handle that in Haskell properly, so keeping as Map

type Aunt = Map String Int

baseSue = parseRow "children: 3 \n cats: 7 \n samoyeds: 2 \n pomeranians: 3 \n akitas: 0 \n vizslas: 0 \n goldfish: 5 \n trees: 3 \n cars: 2 \n perfumes: 1"

asPart :: [String] -> (String,Int)
asPart (x:y:[]) = (x, read y)

getVals :: Ord a => Map a b -> [a] -> [Maybe b]
getVals m xs = map (flip Map.lookup m ) xs

maybeEq :: Aunt -> Aunt -> Bool
maybeEq a b = (getVals a sames) == (getVals b sames)
    where 
        sames = sort . Set.toList $ Set.intersection (Map.keysSet a) (Map.keysSet b)


parseRow :: String -> Aunt
parseRow = Map.fromList . (map  asPart) . group
    where
        group = (chunksOf 2) .  words .  (filter (/=',')) . (filter (/=':'))


part1 :: String -> Int
part1 = (Map.findWithDefault (-1) "Sue" ) . head .  filter (  maybeEq baseSue  ) . (map parseRow ) . lines

part2 :: String -> Int 
part2 s = 0
