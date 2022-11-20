module Day6 where

import Data.List.Split

newtype Fish = Fish Int deriving (Show)

updateFish :: Fish -> [Fish]
updateFish (Fish i) = if i < 1 then [Fish 6, Fish 8] else [Fish (i - 1)]

solve :: String -> String
solve = show . solveA

solveA :: String -> Int
solveA s = length $ iterate nextday (fishes s) !! 80

fishes :: String -> [Fish]
fishes = map (Fish . read) . splitOn ","

nextday :: [Fish] -> [Fish]
nextday = foldMap updateFish

-- above is too slow for B

-- Given a fish return the # of direct children after N days
children :: Fish -> Int -> Int
children (Fish i) d
  | d <= i = 0
  | otherwise = 1 + (d - i) `div` 6

-- Given a fish return the # of total children after N days
family :: Fish -> Int -> Int
family = undefined
