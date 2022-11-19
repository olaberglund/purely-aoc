module Day4 where

import Data.List
import Parsing
import Text.ParserCombinators.ReadP

type Board = [[Int]]

data Bingo = Bingo [Int] [Board] deriving (Show)

solve :: String -> String
solve s = case parseWith bingoP s of
  Nothing -> "fail"
  Just (Bingo draws boards) -> show $ sum unmarks * lst
    where
      (lst : unmarks) = play draws boards

play :: [Int] -> [Board] -> [Int]
play = play' []
  where
    play' :: [Int] -> [Int] -> [Board] -> [Int]
    play' _ [] _ = []
    play' drawn (d : left) bs =
      let drawn' = drawn ++ [d]
       in case winningBoard drawn' bs of
            Just b -> d : unmarked drawn' b
            Nothing -> play' drawn' left bs

winningBoard :: [Int] -> [Board] -> Maybe Board
winningBoard = find . isWin

bingoP :: ReadP Bingo
bingoP = Bingo <$> rowP ',' <*> (newline *> many (rowP ' ') `sepBy` newline)

isWin :: [Int] -> Board -> Bool
isWin ds b = any (`contains` ds) (transpose b `union` b)

contains :: Eq a => [a] -> [a] -> Bool
contains [] _ = True
contains (x : xs) y = elem x y && contains xs y

unmarked :: [Int] -> Board -> [Int]
unmarked = foldMap . flip (\\)
