module AOC21.Day6 where

import Data.List.Split
import Data.MultiSet (MultiSet, concatMap, fromList, size)

updateFish :: Int -> [Int]
updateFish i = if i < 1 then [6, 8] else [i - 1]

solve :: String -> String
solve = show . solveB

solveA :: String -> Int
solveA s = length $ iterate nextday (fishes s) !! 80

fishes :: String -> [Int]
fishes = map read . splitOn ","

nextday :: [Int] -> [Int]
nextday = foldMap updateFish

-- below is not done by me

simulate :: MultiSet Int -> MultiSet Int
simulate = Data.MultiSet.concatMap (\i -> if i == 0 then [6, 8] else [i - 1])

readInput :: String -> MultiSet Int
readInput = fromList . map read . splitOn ","

solveA' :: String -> String
solveA' = show . size . (!! 80) . iterate simulate . readInput

solveB :: String -> String
solveB = show . size . (!! 256) . iterate simulate . readInput
