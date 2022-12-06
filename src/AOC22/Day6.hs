module AOC22.Day6 where

import Data.Containers.ListUtils (nubOrd)

solve :: String -> String
solve = show . marker . sliding 14

sliding :: Int -> String -> [String]
sliding _ [] = []
sliding n s = take n s : sliding n (drop 1 s)

marker :: [String] -> Int
marker [] = 0
marker (s : ss) = if nubOrd s == s then length s else 1 + marker ss
