{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Parsing where

import Data.Char (isSpace)
import GHC.Unicode (isDigit)
import Text.ParserCombinators.ReadP

parseWith :: ReadP a -> String -> Maybe a
parseWith p s = case [a | (a, rest) <- readP_to_S p s, all isSpace rest] of
  [a] -> Just a
  _failed -> Nothing

newline :: ReadP String
newline = string "\n"

number :: ReadP Int
number = optional (char ' ') *> (read <$> munch1 isDigit)

rowP :: Char -> ReadP [Int]
rowP c = number `sepBy1` char c <* newline
