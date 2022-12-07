module AOC22.Day6 where

import Data.Containers.ListUtils (nubOrd)
import Utils (sliding)

solve :: String -> String
solve = show . marker . sliding 14

marker :: [String] -> Int
marker [] = 0
marker (s : ss) = if nubOrd s == s then length s else 1 + marker ss
