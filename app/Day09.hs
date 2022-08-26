module Day09 where

import Data.Map (Map)
import qualified Data.Map as Map

--import Data.Set (Set)
--import qualified Data.Set as Set

import Data.List (nub, union, delete, sort)

import Data.Maybe (catMaybes, fromJust, fromMaybe  )

import Data.Function.Memoize
-- doesn't work on dict or set it seems


import Debug.Trace



type Place = String
type Cost = Int

type Paths = Map Place [(Place, Cost)]


parseRow :: String -> (Place, Place, Cost)
parseRow s = ( (xs!!0),(xs!!2), (read (xs!!4) ) )
    where xs = words s

makeGraph :: [String] -> Paths
makeGraph = Map.fromListWith  (++) . rows . (map  parseRow)

rows :: [(Place,Place, Cost )] -> [(Place, [(Place, Cost)]) ]
rows xs = map asRow ( xs ++ (map flipPath xs)   )

asRow :: (Place,Place, Cost ) -> (Place, [(Place, Cost)])
asRow (a,b,c) = (a, [(b,c)]  )

flipPath  :: (Place,Place, Cost ) -> (Place,Place, Cost )
flipPath (a,b,c) = (b,a,c)

requiredCities :: [String] -> [Place]
requiredCities xs =  sort $   ( nub $ map (!!0) ys   ) `union`  (nub $ map (!!2) ys   )
    where ys = map words xs



travellingSalesman :: Paths -> [Place] -> Maybe Int
travellingSalesman paths toSee = minimum' $ catMaybes $ traceShowId  $ map (travel toSee) (Map.keys paths )
    where travel' :: [Place] -> Place -> Maybe Int
          travel' [] _ = Just 0
          travel' toSee' place = minimum' $ catMaybes $ traceShow (toSee, posses,place,res  ) res
            where   toSee = delete  place toSee'
                    posses = filter (\x ->  (elem  (fst x) toSee )  ) (fromMaybe [] $ Map.lookup place paths)
                    res =   [ fmap (cost+) (travel (delete place' toSee ) place')   | (place', cost)  <-  posses  ]
                    
          
          travel = memoize2 travel'
         
minimum' :: Ord a => [a] -> Maybe a
minimum' [] = Nothing
minimum' xs = Just $ minimum xs


part1 :: [String] -> Int
part1 xs = fromJust  $ travellingSalesman (makeGraph xs) (requiredCities xs)

part2 :: [String] -> Int
part2 xs = 0