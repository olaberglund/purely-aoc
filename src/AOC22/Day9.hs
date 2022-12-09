{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC22.Day9 where

import Data.Foldable
import Data.List (scanl')
import qualified Data.MultiSet as MS
import Utils (Pos)

-- multiset to represent places the tail has visited

-- | Rope: (head, tail)
type Rope = (Pos, Pos)

data Move = Move String Int

start :: Rope
start = ((0, 0), (0, 0))

solve :: String -> String
solve =
  show
    -- . foldl' (\acc o -> if snd o > 0 then acc + 1 else acc) 0
    . MS.toOccurList
    . MS.fromList
    . scanl' simulate start
    . parse

parse :: String -> [Move]
parse = map mkMove . lines

mkMove :: String -> Move
mkMove s = let [dir, len] = words s in Move dir (read len)

simulate :: Rope -> Move -> [Rope]
simulate (h, t) m = if dist (move h m, t) > 1 then (move h m, h) else (move h m, t)

track :: Rope -> MS.MultiSet Pos -> MS.MultiSet Pos
track = MS.insert . snd

dist :: Rope -> Int
dist ((x1, y1), (x2, y2)) = floor $ sqrt $ fromIntegral ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

move :: Pos -> Move -> Pos
move (x, y) = \case
  (Move "U" _) -> (x, y + 1)
  (Move "D" _) -> (x, y - 1)
  (Move "R" _) -> (x + 1, y)
  (Move "L" _) -> (x - 1, y)
