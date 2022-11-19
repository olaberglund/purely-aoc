{-# LANGUAGE NamedFieldPuns #-}

module Day4 where

import Data.Char (isDigit, isSpace)
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))

type Board = [[String]]

data Bingo = Bingo {draws :: [String], boards :: [Board]} deriving Show

parseWith :: ReadP a -> String -> Maybe a
parseWith p s = case [a | (a, rest) <- parse p s, all isSpace rest] of
  [a] -> Just a
  _failed -> Nothing

solve :: String -> Maybe Bingo
solve = parseWith bingoP

parse :: ReadP a -> ReadS a
parse = readP_to_S

bingoP :: ReadP Bingo
bingoP = do
  draws <- drawsP
  boards <- boardsP
  return $ Bingo {draws, boards}

newline :: ReadP String
newline = string "\n"

drawsP :: ReadP [String]
drawsP = line ','

boardRowP :: ReadP [String]
boardRowP = line ' '

line :: Char -> ReadP [String]
line c = sepBy number (char c) <* string "\n"

boardsP :: ReadP [Board]
boardsP = many boardP 

boardP :: ReadP Board
boardP = string "\n" *> many boardRowP

nbrs :: [Char]
nbrs = "93,18,74,26,98,2,94,23,15\n"

b :: String
b = "\n1 2 3 4 5\n1 2 3 4 5\n"

bs :: String
bs = "\n1 2 3 4 5\n1 2 3 4 5\n\n1 2 3 4 5\n1 2 3 4 5\n"

full :: String
full = "1,2,3,4,5,6\n\n17  8 90 62 17\n98 88 49 41 74\n66  9 83 69 91\n33 57  3 71 43\n11 50  7 10 28\n\n6 34 13  5  9\n" -- \n50 21 66 77  3\n60 74 40 12 33\n69 57 99 18 95\n70 72 49 71 87\n"

number :: ReadP String
number = optional (char ' ') *> munch1 isDigit
