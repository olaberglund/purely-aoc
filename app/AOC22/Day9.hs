{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC22.Day9 where

import Data.List (nub)
import Linear (V2 (V2))

data Dir = R | D | L | U deriving (Show)

data Move = Move Dir Int deriving (Show)

type Input = [Move]

-- below is mostly not mine

offset :: Dir -> V2 Integer
offset = \case
  R -> V2 1 0
  D -> V2 0 (-1)
  L -> V2 (-1) 0
  U -> V2 0 1

parse :: String -> [Move]
parse = map mkMove . lines

mkMove :: String -> Move
mkMove s =
  let [dir, len] = words s
      n = read len
   in case dir of
        "D" -> Move D n
        "U" -> Move U n
        "L" -> Move L n
        "R" -> Move R n

offsets :: Move -> [V2 Integer]
offsets (Move dir steps) = replicate steps $ offset dir

allOffsets :: [Move] -> [V2 Integer]
allOffsets = (>>= offsets)

pathFrom :: V2 Integer -> [V2 Integer] -> [V2 Integer]
pathFrom = scanl (+)

moveTail :: (Ord a, Num a) => V2 a -> V2 a -> V2 a
moveTail tailPos headPos =
  let d@(V2 dx dy) = headPos - tailPos
   in if abs dx > 1 || abs dy > 1 then tailPos + signum d else tailPos

headPath :: [Move] -> [V2 Integer]
headPath = pathFrom (V2 0 0) . allOffsets

tailPath :: [V2 Integer] -> [V2 Integer]
tailPath = scanl1 moveTail

numVisited :: [V2 Integer] -> Int
numVisited = length . nub

part1 :: [Move] -> Int
part1 = numVisited . tailPath . headPath

part2 :: [Move] -> Int
part2 = numVisited . (!! 9) . iterate tailPath . headPath

solve :: String -> String
solve = show . part2 . parse

-- Below is mine, got stuck.

-- module AOC22.Day9 where

-- import Utils (Pos)

-- -- multiset to represent places the tail has visited

-- -- | Rope: (head, tail)
-- type Rope = (Pos, Pos)

-- data Move = Move String Int

-- start :: Rope
-- start = ((0, 0), (0, 0))

-- solve :: String -> [Rope]
-- solve = undefined

-- simulate :: Move -> Rope -> Rope
-- simulate m (h, t) =
--   let p = move h m
--    in if dist (p, t) > 1 then (p, h) else (p, t)

-- dist :: Rope -> Int
-- dist ((x1, y1), (x2, y2)) = floor $ sqrt $ fromIntegral ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

-- move :: Pos -> Move -> Pos
-- move (x, y) = \case
--   (Move "U" _) -> (x, y + 1)
--   (Move "D" _) -> (x, y - 1)
--   (Move "R" _) -> (x + 1, y)
--   (Move "L" _) -> (x - 1, y)
