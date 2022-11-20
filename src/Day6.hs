module Day6 where

import Data.List.Split

newtype Fish = Fish Int deriving (Show)

updateFish :: Fish -> [Fish]
updateFish (Fish i) = if i < 1 then [Fish 6, Fish 8] else [Fish (i - 1)]

solve :: String -> String
solve = show . solveA

nextday :: [Fish] -> [Fish]
nextday = foldMap updateFish

solveA :: String -> Int
solveA s = length $ iterate nextday (fishes s) !! 80

fishes :: String -> [Fish]
fishes = map (Fish . read) . splitOn ","

-- above is too slow for B

-- Given a fish return the # of children after N days
children :: Fish -> Int -> Int
children (Fish i) = undefined
