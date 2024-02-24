module AOC21.Day8 where

import Data.Foldable (foldl')
import Data.List.Split (splitWhen)

type Input = [String]

type Output = [String]

numbers :: [Integer]
numbers = [0x1111110, 0x0110000, 0x1101101, 0x1111001, 0x0110011, 0x1011011, 0x1011111, 0x1110000, 0x1111111, 0x1111011]

parse :: String -> [(Input, Output)]
parse = map ((\xs -> (head xs, last xs)) . splitWhen (== "|") . words) . lines

solve :: String -> String
solve = show . sum . map count1478 . parse

count1478 :: (Input, Output) -> Int
count1478 (_, o) =
  foldl' (\acc s -> if length s `elem` [2, 3, 4, 7] then acc + 1 else acc) 0 o
