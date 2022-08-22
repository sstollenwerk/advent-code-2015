module Day07 where


import Data.Either
import Data.Bits
import Data.Word
import Data.Maybe (fromJust)
import Data.Char (isDigit )

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Function.Memoize



type Key = String

type Number = Word16 

type Value = Either Number Key



data Command = D1 Value | D2 ( (Number -> Number),  Value  ) | D3 ( (Number -> Number -> Number),  Value,  Value )

type Row = (Key, Command)

type CircuitState = Map Key Number

type Commands = Map Key Command

parse :: String -> Row
parse s =  (key, makeCommand xs)
    where xs = words s
          key = xs!!((length xs) - 1)

makeCommand :: [String] -> Command
makeCommand xs
    | length xs == 3 =  D1 $ makeValue $xs!!0
    | length xs == 4 =  D2  (complement , makeValue (xs!!1))
    | length xs == 5 =  D3  (makeFunc (xs!!1) , makeValue (xs!!0), makeValue (xs!!2) )
    

makeFunc :: String ->  (Number -> Number -> Number)
makeFunc "AND" = (.&.)
makeFunc "OR" = (.|.)
makeFunc "LSHIFT" = shiftL'
makeFunc "RSHIFT" = shiftR'

shiftL' :: Number -> Number -> Number
shiftL' a b = shiftL a (fromIntegral b)

shiftR' :: Number -> Number -> Number
shiftR' a b = shiftR a (fromIntegral b)


makeValue :: String -> Value
makeValue s
    | all isDigit s = Left $ fromIntegral $ read s
    | otherwise = Right s


findWire :: Commands -> Key -> Number
findWire s k = findWire' (fromJust $ Map.lookup k s)
    where 
        findWire' :: Command -> Number
        findWire'  (D1 v) =  getVal v
        findWire'  (D2 (f, v)) =  f (getVal v)
        findWire'  (D3 (f, v1, v2)) =  f (getVal v1) (getVal v2)


        getVal' :: Value -> Number
        getVal' (Left n) = n
        getVal' (Right k) = findWire' (fromJust $ Map.lookup k s)

        getVal = memoize getVal'

err a b = error ""

part1 :: [Row] -> Key -> Number
part1 xs = (findWire (Map.fromListWith  err xs) )

part2 :: [Row] -> Key -> Number
part2 xs k = findWire commands' k
    where commands = Map.fromListWith  err xs
          r = part1 xs k
          commands' = Map.insert "b" (D1 (Left r) ) commands
