module Day19 where



import Data.List (intercalate, maximumBy)

import Data.Tuple (swap)

import Data.List.Split (splitOn, splitPlacesBlanks, split, keepDelimsR,onSublist  )

import Data.Maybe (catMaybes, fromJust)

import Data.Function ( on )

import           Data.Set        (Set)
import qualified Data.Set        as Set

import Debug.Trace (traceShowId)




type Replaces = (String, String)

todo = error "todo"

asRepl :: String -> Replaces
asRepl s = ( (r!!0), (r!!1) )
    where
        r = splitOn " => " s

replacements :: String -> Replaces  -> [String]
replacements s (a, b) = map replaces (parts groups)
    where 
        groups = splitOn a s
        replaces xs = (intercalate a (xs!! 0)) ++ b ++ (intercalate a (xs!! 1))

parts :: [a] -> [[[a]]]
parts s = [splitPlacesBlanks [i,n] s | i <- [1..(n-1)]]
    where
        n = length s

allReplacements ::  [Replaces] -> String -> Set String
allReplacements  repls k = Set.unions $ map (Set.fromList . (replacements k)  ) repls

reduce :: [Replaces] ->  Set String -> (String, Int)
reduce repls posses = (res, (length vals) - 1   )
    where 
        step :: Set String -> Set String
        step = Set.unions . (Set.map (allReplacements repls) )
        vals = takeWhile (not . Set.null  ) $  iterate step posses
        res = Set.elemAt 0  (last vals)

steps :: [Replaces] ->   [String] -> Int
steps repls vals = go (  vals)
        -- apparently only works with popright not popleft.
    where 
        red x = reduce repls (Set.singleton  x)
        go :: [String] -> Int
        go [] = 0
        go (x:[]) =  snd $ traceShowId $ red x
        go (x:y:xs) = amt + go ((a++y):xs)
            where
                (a,amt) = traceShowId $ red  (traceShowId x)

parse :: String -> ([Replaces], String)
parse s = ( (map asRepl repl), mol )
    where
        parts = lines s
        mol = last parts
        repl = filter ( (not ) . null ) $ init parts



part1 :: String -> Int
part1 = length . (uncurry  allReplacements) . parse




part2 :: String -> Int
part2 s = steps repls segments
    where 
        (repls', k) =  parse s
        repls = map swap repls'
        suffix  = last   (maximumBy (compare `on` length) (map fst repls))

        segments = traceShowId $ split (keepDelimsR $ onSublist   "Ar") k
        -- in input I have "r" is suffix of all long substitutions but "r" doesn't appear in subtitutions

