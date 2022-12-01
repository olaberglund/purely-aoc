module AOC22.Day1 where

import Control.Monad
import Data.List (sortBy)
import Data.List.Split (splitWhen)
import Data.Maybe (catMaybes, isNothing)
import Text.Read (readMaybe)

solve :: String -> String
solve = show . solveB . lines

calories :: [String] -> [Int]
calories = map (sum . catMaybes) . splitWhen isNothing . map readMaybe

solveA :: [String] -> Int
solveA = maximum . calories

solveB :: [String] -> Int
solveB = sum . take 3 . sortBy (flip compare) . calories
