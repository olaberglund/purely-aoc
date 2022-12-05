{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC22.Day5 where

import Data.Char (isAlpha)
import Data.List.Split (wordsBy)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import GHC.Unicode (isSpace)

type Crate = Char

type Stack = [Crate]

type Crates = Map.Map Int Stack

data Command = Command Int Int Int deriving (Show)

solve :: String -> String
solve s = show $ map head $ Map.elems $ foldl moveB (crates s) (commands s)

crates :: String -> Crates
crates s = Map.fromList $ zip [1 ..] [dropWhile isSpace $ map (!! i) (take 8 (lines s)) | i <- [1, 5 .. 33]]

commands :: String -> [Command]
commands = map mkCommand . drop 10 . lines

-- move n from to
moveBy :: ([Crate] -> [Crate]) -> Crates -> Command -> Crates
moveBy f cs (Command n from to) = fromMaybe cs $ do
  stack <- Map.lookup from cs
  let lifted = f $ take n stack
      cs' = Map.adjust (drop n) from cs
   in return $ Map.adjust (lifted ++) to cs'

moveA, moveB :: Crates -> Command -> Crates
moveA = moveBy reverse
moveB = moveBy id

mkCommand :: String -> Command
mkCommand s = let [n, x, y] = nums in Command n x y
  where
    nums :: [Int]
    nums = map read $ wordsBy (\c -> isAlpha c || isSpace c) s
