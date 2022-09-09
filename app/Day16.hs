module Day16 where

import           Data.List.Split (splitOn, chunksOf )
import           Data.List (sort, and )

import           Data.Map        (Map)
import qualified Data.Map        as Map

import           Data.Set        (Set)
import qualified Data.Set        as Set

import           Data.Maybe (fromJust)



-- would use dataclass in python - not sure how to handle that in Haskell properly, so keeping as Map

type Aunt = Map String Int
type PossAunt = Map String (Int -> Bool)

baseSue = parseRow "children: 3 \n cats: 7 \n samoyeds: 2 \n pomeranians: 3 \n akitas: 0 \n vizslas: 0 \n goldfish: 5 \n trees: 3 \n cars: 2 \n perfumes: 1"


baseSue2 = Map.fromList [("akitas",(==0)),("cars",(==2)),("cats",(>7)),("children",(<3)),("goldfish",(<5)),("perfumes",(==1)),("pomeranians",(<3)),("samoyeds",(==2)),("trees",(>3)),("vizslas",(==0))]


asPart :: [String] -> (String,Int)
asPart (x:y:[]) = (x, read y)

parseRow :: String -> Aunt
parseRow = Map.fromList . (map  asPart) . group
    where
        group = (chunksOf 2) .  words .  (filter (/=',')) . (filter (/=':'))


getVals :: Ord a => Map a b -> [a] -> [b]
getVals m xs = map (fromJust . flip Map.lookup m ) xs

maybeEq :: Aunt -> Aunt -> Bool
maybeEq a b = (getVals a sames) == (getVals b sames)
    where 
        sames = sort . Set.toList $ Set.intersection (Map.keysSet a) (Map.keysSet b)

--call ::  (a -> b) ->  a   -> b
--call f a = f a
-- apparently this is ($)

maybeEq2 :: PossAunt -> Aunt -> Bool
maybeEq2 a b  = and $ zipWith ($)  (getVals a sames)  (getVals b sames)
    where 
        sames = sort . Set.toList $ Set.intersection (Map.keysSet a) (Map.keysSet b)


part1 :: String -> Int
part1 = (Map.findWithDefault (-1) "Sue" ) . head .  filter (  maybeEq baseSue  ) . (map parseRow ) . lines

part2 :: String -> Int 
part2 = (Map.findWithDefault (-1) "Sue" ) . head .  filter (  maybeEq2 baseSue2  ) . (map parseRow ) . lines
