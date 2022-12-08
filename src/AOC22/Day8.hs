{-# LANGUAGE LambdaCase #-}

module AOC22.Day8 where

import Data.Char (digitToInt)
import Data.List
import qualified Data.Map as Map
import Utils (range)

type Tree = Int

type Pos = (Int, Int)

data Dir = U | D | L | R

type Forest = Map.Map Pos Tree

solve :: String -> String
solve = solveB . mkForest . lines

solveB :: Forest -> String
solveB forest = show $ maximum $ map (treeScore forest) (Map.keys forest)

solveA :: Forest -> String
solveA forest = show $ length $ filter (== True) $ map (`isVisible` forest) $ Map.keys forest

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

treeScore :: Forest -> Pos -> Int
treeScore for pos = product $ map (length . takeWhile (< Map.lookup pos for) . trees for pos) [U, D, L, R]

line :: Pos -> Dir -> [Pos]
line (x, y) = \case
  U -> [(x, y') | y /= 0, y' <- range (y - 1) 0]
  D -> [(x, y') | y' <- [y + 1 ..]]
  L -> [(x', y) | x /= 0, x' <- range (x - 1) 0]
  R -> [(x', y) | x' <- [x + 1 ..]]
