module Day9 where

import Data.Char (digitToInt)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Ix (Ix(range))

type Matrix a = Map.Map Pos a

type Pos = (Int, Int)

solve :: String -> String
solve s = show $ length [lp | lp <- coordinates, isLowPoint (mkMatrix s) lp]

mkMatrix :: String -> Matrix Int
mkMatrix = Map.fromList . zip coordinates . map digitToInt . concat . lines

coordinates :: [Pos]
coordinates = range ((0,0), (99,99))

isLowPoint :: Matrix Int -> Pos -> Bool
isLowPoint m p = case Map.lookup p m of
  Just x -> all (> x) (surroundings m p)
  Nothing -> False

surroundings :: Matrix Int -> Pos -> [Int]
surroundings m (x, y) =
  catMaybes
    [ Map.lookup (x, y + 1) m,
      Map.lookup (x, y - 1) m,
      Map.lookup (x - 1, y) m,
      Map.lookup (x + 1, y) m
    ]
