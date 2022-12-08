{-# LANGUAGE LambdaCase #-}

module AOC22.Day8 where

import Data.Char (digitToInt)
import Data.Functor
import Data.List
import qualified Data.Map as Map

type Tree = Int

type Pos = (Int, Int)

data Dir = U | D | L | R

type Forest = Map.Map Pos Tree

solve :: String -> String
solve inp = show $ length $ filter (== True) $ map (`isVisible` forest) (coords (lines inp))
  where
    forest :: Forest
    forest = mkForest $ lines inp

coords :: [String] -> [Pos]
coords inp = [(x, y) | y <- [0 .. length inp - 1], x <- [0 .. length (transpose inp) - 1]]

mkForest :: [String] -> Forest
mkForest inp = Map.fromList $ zip (coords inp) (map digitToInt $ concat inp)

isVisible :: Pos -> Forest -> Bool
isVisible pos for = any (allShorter (Map.lookup pos for) . trees for pos) [U, D, L, R]
  where
    allShorter t = all (< t)

trees :: Forest -> Pos -> Dir -> [Maybe Tree]
trees for p dir = takeWhile (/= Nothing) $ map (`Map.lookup` for) (line p dir)

line :: Pos -> Dir -> [Pos]
line (x, y) = \case
  U -> [(x, y') | y /= 0, y' <- [0 .. y - 1]]
  D -> [(x, y') | y' <- [y + 1 ..]]
  L -> [(x', y) | x /= 0, x' <- [0 .. x - 1]]
  R -> [(x', y) | x' <- [x + 1 ..]]

input = readFile "./inputs/22/input_8.txt"

forest = input <&> (mkForest . lines)
