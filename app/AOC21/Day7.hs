module AOC21.Day7 where

import Data.List (sort)
import Data.List.Split (splitOn)

solve :: String -> String
solve = show . solveB . map read . splitOn ","

median :: [Int] -> Int
median xs = sort xs !! (length xs `div` 2)

cost :: (Int -> Int) -> Int -> [Int] -> Int
cost f h = sum . map f

totCost :: Int -> [Int] -> Int
totCost h = cost (diff h) h

incCost :: Int -> [Int] -> Int
incCost h = cost (arithmSum . diff h) h

arithmSum :: Int -> Int
arithmSum n = n * (n + 1) `div` 2

diff :: Int -> Int -> Int
diff x1 x2 = abs $ x1 - x2

solveA :: [Int] -> Int
solveA = totCost . median <*> id

solveB :: [Int] -> Int
solveB = incCost . mean <*> id

mean :: [Int] -> Int
mean = div . sum <*> length
