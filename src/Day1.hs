module Day1 (solve) where

parse :: String -> [Int]
parse = map read . words

solve :: String -> String
solve = solveB . parse

solveA :: [Int] -> String
solveA = show . increments

solveB :: [Int] -> String
solveB = solveA . slidingSum 3

increments :: [Int] -> Int
increments [x1, x2] = if x2 > x1 then 1 else 0
increments (x1 : x2 : xs) = if x2 > x1 then 1 + increments (x2 : xs) else increments (x2 : xs)
increments _ = 0

slidingSum :: Int -> [Int] -> [Int]
slidingSum n xs
  | length xs < n = []
  | otherwise = sum (take n xs) : slidingSum n (tail xs)
