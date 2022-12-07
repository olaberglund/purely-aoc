{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module AOC22.Day7 where

import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Ord (comparing)

type Path = String

type Output = String

type FileSystem = Map.Map Path [File]

data File = Single Path !Int | Dir Path deriving (Show)

data State = State {wd :: Path, fs :: FileSystem}

solve :: String -> String
solve s = show $ solveB dirs
  where
    dirs :: [[File]]
    dirs = Map.elems fsys

    fsys :: FileSystem
    fsys = fs $ mkState s

    free :: Int
    free = 70000000 - du fsys (Dir "/")

    dirSize :: [File] -> Int
    dirSize = sum . map (du fsys)

    solveA :: [[File]] -> Int
    solveA = foldl' (\acc dir -> if dirSize dir <= 100000 then acc + dirSize dir else acc) 0

    solveB :: [[File]] -> Int
    solveB = dirSize . minimumBy (comparing dirSize) . filter (\dir -> free + dirSize dir >= 30000000)

mkState :: String -> State
mkState = foldl' handle (State {wd = "", fs = Map.empty}) . lines

du :: FileSystem -> File -> Int
du _ (Single _ size) = size
du fsys (Dir p) = case Map.lookup p fsys of
  Just files -> sum $ map (du fsys) files
  Nothing -> 0

append :: Path -> Path -> Path
append "/" p = "/" <> p
append p1 p2 = p1 <> "/" <> p2

cd :: Path -> State -> State
cd "/" st = st {wd = "/"}
cd ".." st = st {wd = back (wd st)}
cd p st = st {wd = append (wd st) p}

back :: Path -> Path
back "/" = "/"
back p = intercalate "/" $ init $ splitOn "/" p

handle :: State -> Output -> State
handle st s = case words s of
  ["$", "cd", p] -> cd p st
  ["$", "ls"] -> st
  ["dir", name] -> st {fs = Map.insertWith (++) (wd st) [Dir (wd st `append` name)] (fs st)}
  [size, name] -> st {fs = Map.insertWith (++) (wd st) [Single (wd st `append` name) (read size)] (fs st)}
