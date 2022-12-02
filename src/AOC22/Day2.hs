{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}

module AOC22.Day2 where

solve :: String -> String
solve = show . sum . map matchB . lines 

matchA :: String -> Int
matchA = \case 
  "A X" -> 4
  "B X" -> 1
  "C X" -> 7
  "A Y" -> 8
  "B Y" -> 5
  "C Y" -> 2
  "A Z" -> 3
  "B Z" -> 9
  "C Z" -> 6

matchB :: String -> Int
matchB = \case 
  "A X" -> 3
  "B X" -> 1
  "C X" -> 2
  "A Y" -> 4
  "B Y" -> 5
  "C Y" -> 6
  "A Z" -> 8
  "B Z" -> 9
  "C Z" -> 7
