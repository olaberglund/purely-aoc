{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module AOC21.Parsing where

import Data.Char (isSpace)
import GHC.Unicode (isDigit)
import Text.ParserCombinators.ReadP

newline :: ReadP String
newline = string "\n"

number :: ReadP Int
number = optional (char ' ') *> (read <$> munch1 isDigit)

rowP :: Char -> ReadP [Int]
rowP c = number `sepBy1` char c <* newline
