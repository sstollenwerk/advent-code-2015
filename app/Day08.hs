module Day08 where

import Data.Char (ord, isHexDigit)

import qualified Data.Text as T
import Data.List (intercalate)
import Data.List.Split (splitOn)

data Value = Slash | Quote | X | Hex | Other deriving (Eq,Ord,Enum,Show)


asVal :: Char -> Value
asVal '\\' = Slash
asVal '"' = Quote
asVal 'x' = X
asVal c
    | isHexDigit c = Hex
    | otherwise = Other

stringCode :: [a] -> Int
stringCode = length

stringMemory :: String -> Int
--stringMemory val = (length (read val :: String))
-- doesn't work as expected
-- e.g. "\xa8br"  should give [168,98,114] but gives [2699,114]
stringMemory = length . adjust


adjust s = res
    where xs = (init . tail) $ map asVal s
          a = replace [Slash,Slash] [Other] xs
          b = replace [Slash,Quote] [Other] a
          res = replace [Slash,X, Hex, Hex] [Other] b


excSize s = (stringCode s) - (stringMemory s)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace bef aft val = intercalate aft  (splitOn bef val)

occurances :: Eq a => a -> [a] -> Int
occurances k = length . filter (==k)


part1 :: [String] -> Int
part1 = sum . (map excSize)

resize :: String -> Int
resize s = 2 + (occurances '"'  s) + (occurances '\\'  s) 

part2 :: [String] -> Int
part2 = sum .  (map resize)