module AOC22.Day3 where

import Data.Foldable (foldl')
import Data.List (elemIndex, intersect)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)

solve :: String -> String
solve = show . foldl' sumByPrio 0 . solveB . lines

solveA :: [String] -> [[Char]]
solveA = map $ uncurry intersect . compartmentalize

solveB :: [[Char]] -> [[Char]]
solveB = map (foldl1 intersect) . chunksOf 3

sumByPrio :: Int -> String -> Int
sumByPrio sum s = priority (head s) + sum

compartmentalize :: String -> (String, String)
compartmentalize s = splitAt (length s `div` 2) s

priority :: Char -> Int
priority c = fromMaybe 0 (elemIndex c (['a' .. 'z'] ++ ['A' .. 'Z'])) + 1
