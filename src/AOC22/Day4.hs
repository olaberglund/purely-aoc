{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC22.Day4 where

import Data.List
import Data.List.Split (splitOn)

solve :: String -> String
solve = solveWith overlaps

solveWith :: ([Int] -> [Int] -> Bool) -> String -> String
solveWith f = show . foldl (\sum [a, b] -> if f (sections a) (sections b) then sum + 1 else sum) 0 . map (splitOn ",") . lines

sections :: String -> [Int]
sections s =
  let [start, end] = map read (splitOn "-" s)
   in [start .. end]

isIn :: Eq a => [a] -> [a] -> Bool
isIn xs ys = intersection == xs || intersection == ys
  where
    intersection = xs `intersect` ys

overlaps :: Eq a => [a] -> [a] -> Bool
overlaps xs ys = xs `intersect` ys /= []
