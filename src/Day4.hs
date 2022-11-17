module Day4 where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

{-

<file> := <line>*
<line> := <drawn> | <boardRow>
  -}

parse :: ReadP a -> ReadS a
parse = readP_to_S

numbers :: String
numbers = "1,2,3,4,5,6,7"

row :: String
row = "1 2 3 4 5 6 7"

drawn :: ReadP [String]
drawn = sepBy number (char ',')

boardRow :: ReadP [String]
boardRow = sepBy number (char ' ')

number :: ReadP String
number = munch1 isDigit
