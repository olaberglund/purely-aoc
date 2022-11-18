{-# LANGUAGE NamedFieldPuns #-}

module Day4 where

import Data.Char (isDigit, isSpace)
import Text.ParserCombinators.ReadP

type Board = [[String]]

data Bingo = Bingo {draws :: [String], boards :: [Board]}

parseWith :: ReadP a -> String -> Maybe a
parseWith p s = case [a | (a, rest) <- parse p s, all isSpace rest] of
  [a] -> Just a
  _failed -> Nothing

parse :: ReadP a -> ReadS a
parse = readP_to_S

bingoP :: ReadP Bingo
bingoP = do
  draws <- drawsP
  boards <- boardsP
  return $ Bingo {draws, boards}

drawsP :: ReadP [String]
drawsP = line ','

boardRowP :: ReadP [String]
boardRowP = line ' '

line :: Char -> ReadP [String]
line c = sepBy number (char c) <* string "\n"

boardsP :: ReadP [Board]
boardsP = undefined

boardP :: ReadP Board
boardP = many boardRowP

board :: String
board = "6 34 13 5 9\n50 21 66 77 3\n60 74 40 12 33\n69 57 99 18 95\n70 72 49 71 87\n"

number :: ReadP String
number = munch isDigit

test1 :: String
test1 = "1,2,3,4,5,6,7\n"

test2 :: String
test2 = "1 2 3 4 5 6 7\n"

test12 :: [Char]
test12 = test1 ++ test2
