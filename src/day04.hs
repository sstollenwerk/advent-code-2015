module Main where

import Data.Word (Word8 )

import Data.Char (ord)
import Data.List (isPrefixOf)

import Data.Digest.Pure.MD5 (md5 )
import Data.ByteString.Lazy (ByteString, pack)




part1 :: String -> Int
part1 s =  head $ filter ( valid1 . (s++) . show    ) [ 1..]

part2 :: String -> Int
part2 s =  head $ filter ( valid2 . (s++) . show    ) [ 1..]


valid1 :: String -> Bool
valid1 = (isPrefixOf "00000") . parse

valid2 :: String -> Bool
valid2 = (isPrefixOf "000000") . parse


parse :: String -> String
parse = show . md5 . pack . map ( fromIntegral . ord )

main :: IO ()
main = do 
          file <- readFile ("../data/04.txt")
          putStrLn ( "p1: " ++ (show $ part1 file ) )
          putStrLn ( "p2: " ++ (show $ part2 file ) )

