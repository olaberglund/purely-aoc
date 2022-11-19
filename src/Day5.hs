module Day5 where

import Data.Char (isDigit)
import Data.Containers.ListUtils (nubOrd)
import Data.List (nub)
import Data.Map as Map hiding (foldr, map)
import Parsing (newline, parseWith)
import Text.ParserCombinators.ReadP

type Coordinate = (Int, Int)

data Line = Line Coordinate Coordinate deriving (Show)

type OceanFloor = Map.Map Coordinate Int

solve :: String -> String
solve s = case parseWith (many lineP) s of
  Just ls -> show $ Map.foldl (\acc i -> if i > 1 then acc + 1 else acc) (0 :: Int) (mkFloor ls)
  Nothing -> "parsing failed"

lineP :: ReadP Line
lineP = Line <$> coordP <* string " -> " <*> coordP <* newline

coordP :: ReadP Coordinate
coordP = (,) <$> nbrsP <* char ',' <*> nbrsP
  where
    nbrsP :: ReadP Int
    nbrsP = read <$> munch1 isDigit

-- only keep vertical/horizontal lines
fromLine :: Line -> [Coordinate]
fromLine (Line (x1, y1) (x2, y2)) = [(x, y) | y1 == y2 || x1 == x2, x <- range x1 x2, y <- range y1 y2]

-- keep also 45 deg lines
fromLine' :: Line -> [Coordinate]
fromLine' l@(Line (x1, y1) (x2, y2)) = nubOrd $ fromLine l ++ zip (range x1 x2) (range y1 y2)

range :: Int -> Int -> [Int]
range i1 i2
  | i1 < i2 = [i1 .. i2]
  | otherwise = reverse [i2 .. i1]

mkFloor :: [Line] -> OceanFloor
mkFloor ls = foldr (\c m -> Map.insertWith (\_ i -> i + 1) c 1 m) Map.empty (concatMap fromLine' ls)
