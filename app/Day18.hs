module Day18 where


import           Data.Map.Lazy        (Map)
import qualified Data.Map.Lazy as Map

-- Map operations require ord which Complex doesn't do.

data Position = Position{x:: Int, y:: Int} deriving (Show, Eq, Ord)

type Locations = Map Position Bool


adjs :: Position -> [Position]
adjs (Position a b) = [Position (a+i)  (b+j) |  (i,j) <- vals  ]
    where 
        px = [(-1)..1] 
        vals = filter (/= (0,0)) [ (i,j) |  i <- px, j <- px  ]

asPlaces :: String -> Int -> Locations
asPlaces xs j =   Map.fromList   is
    where
        is =    zip  (map (Position j) [0..]) (map (=='#') xs )

parse :: String -> Locations
parse s = Map.unions  rows
    where
        rows = zipWith asPlaces (lines s) [0..]


lives :: Locations -> Position -> Bool
lives locs p
    | alive p = alives `elem` [2,3]
    | otherwise = alives == 3
    where 
        alive = flip (Map.findWithDefault  False) locs
        alives = length $ filter alive (adjs p)

keepFst :: (a -> c) -> (a -> b -> c)
keepFst f =  go
    where go a b = f a

mapFrom :: Ord a => (a -> b) -> [a] -> Map a b
mapFrom f xs = Map.fromList (zip xs (map f xs)  )

lifeStep :: Locations -> Locations
lifeStep locs = mapFrom (lives locs) (Map.keys locs)

stick :: Locations -> Locations
stick p = foldl (flip (flip Map.insert True) ) p   [(Position 0 0), (Position 0 99), (Position 99 0), (Position 99 99) ]



part1 :: String -> Int
part1 s = length $ Map.filter (==True) $ (iterate lifeStep (parse s) ) !!100


part2 :: String -> Int
part2 = length . Map.filter (==True) . (!!100) . (iterate (stick . lifeStep)) . parse