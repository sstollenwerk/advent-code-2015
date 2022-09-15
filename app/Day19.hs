module Day19 where


import Data.List.Split (splitOn, splitPlacesBlanks )

import Data.List (intercalate)

import Data.Set (Set)
import qualified Data.Set as Set

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



parse :: String -> ([Replaces], String)
parse s = ( (map asRepl repl), mol )
    where
        parts = lines s
        mol = last parts
        repl = filter ( (not ) . null ) $ init parts


part1 :: String -> Int
part1 s = length $ Set.unions $ map (Set.fromList . (replacements k)  ) repls
    where (repls, k) = traceShowId $ parse s

part2 s = 0