module Utils where

import Data.Char (isSpace)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)

parseWith :: ReadP a -> String -> Maybe a
parseWith p s = case [a | (a, rest) <- readP_to_S p s, all isSpace rest] of
  [a] -> Just a
  _failed -> Nothing

sliding :: Int -> String -> [String]
sliding _ [] = []
sliding n s = take n s : sliding n (drop 1 s)

range :: Int -> Int -> [Int]
range i1 i2
  | i1 < i2 = [i1 .. i2]
  | otherwise = reverse [i2 .. i1]
