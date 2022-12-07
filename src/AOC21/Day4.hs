{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}

module AOC21.Day4 where

import AOC21.Parsing
import Data.List
import Text.ParserCombinators.ReadP
import Utils

type Board = [[Int]]

data Bingo = Bingo [Int] [Board] deriving (Show)

solve :: String -> String
solve s = case parseWith bingoP s of
  Nothing -> "fail"
  Just (Bingo draws boards) -> show $ sum unmarks * lst
    where
      (lst : unmarks) = playA draws boards

winningBoards :: [Int] -> [Board] -> [Board]
winningBoards ds = filter (isWin ds)

bingoP :: ReadP Bingo
bingoP = Bingo <$> rowP ',' <*> (newline *> many (rowP ' ') `sepBy` newline)

isWin :: [Int] -> Board -> Bool
isWin ds b = any (`contains` ds) (transpose b `union` b)

contains :: Eq a => [a] -> [a] -> Bool
contains [] _ = True
contains (x : xs) y = elem x y && contains xs y

unmarked :: [Int] -> Board -> [Int]
unmarked = foldMap . flip (\\)

playA :: [Int] -> [Board] -> [Int]
playA ds bs = last $ play [] [] ds bs

playB :: [Int] -> [Board] -> [Int]
playB ds bs = head $ play [] [] ds bs

play :: [Int] -> [(Board, [Int])] -> [Int] -> [Board] -> [[Int]]
play _ winners [] _ = map (\p -> head (snd p) : unmarked (snd p) (fst p)) winners
play drawn winners (d : left) boards =
  let drawn' = d : drawn
   in case winningBoards drawn' boards of
        [] -> play drawn' winners left boards
        newWinners -> play drawn' (map (,drawn') newWinners ++ winners) left (boards \\ newWinners)
