module Main where


import Data.List.Split (splitOn)

data Box = Box { 
  l :: Int  
, w :: Int  
, h :: Int  
} deriving (Show)   

asBox :: String -> Box
asBox s = Box   (xs!!0 )  (xs!!1)  (xs!!2   )
    where xs = map read ( splitOn "x" s)


wrap :: Box -> Int
wrap (Box l w h) = 2* (sum xs) + minimum xs
    where xs = [l*w, l*h, w*h  ]

ribbon (Box l w h) = (foldl1 (*) xs  ) + 2 *(  sum xs - maximum xs   )
    where xs = [l,w,h  ]

part1 :: [String] -> Int
part1 = sum . map (wrap . asBox )

part2 :: [String] -> Int
part2 = sum . map (ribbon . asBox )

main :: IO ()
main = do 
          file <- readFile ("../data/02.txt")
          let parts = lines file
          putStrLn ( "p1: " ++ (show $ part1 parts ) )
          putStrLn ( "p2: " ++ (show $ part2 parts ) )

