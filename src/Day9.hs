module Day9 where

import Data.Char (digitToInt)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (foldl')
import Data.Function (on)
import Data.Ix (Ix (range))
import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import qualified Data.Set as Set
import Debug.Trace (trace)

type Matrix a = Map.Map Pos a

type Pos = (Int, Int)

solve :: String -> String
solve = solveB

solveA :: String -> String
solveA s = show $ foldl' (\acc p -> acc + fromMaybe 0 (Map.lookup p matrix) + 1) 0 (lowpoints matrix)
  where
    matrix = mkMatrix s

solveB :: String -> String
solveB s = show $ product $ take 3 $ sortBy (flip compare) $ map (basin matrix) (lowpoints (mkMatrix s))
  where
    matrix = mkMatrix s

lowpoints :: Matrix Int -> [Pos]
lowpoints m = [lp | lp <- coordinates, isLowPoint m lp]

mkMatrix :: String -> Matrix Int
mkMatrix = Map.fromList . zip coordinates . map digitToInt . concat . lines

coordinates :: [Pos]
coordinates = range ((0, 0), (99, 99))

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

-- given a low point, find levels in basin
basin :: Matrix Int -> Pos -> Int
basin = basin' Set.empty
  where
    basin' :: Set.Set Pos -> Matrix Int -> Pos -> Int
    basin' visited m p
      | Map.lookup p m == Just 9 = 0
      | isNothing (Map.lookup p m) = 0
      | Set.member p visited = 0
      | otherwise = 1 + Set.size $ foldl (\acc pos -> Set.insert acc basin' (Set.insert p s) m) Set.empty (surroundings' p)

surroundings' :: Pos -> [Pos]
surroundings' (x, y) =
  [ (x, y + 1),
    (x, y - 1),
    (x - 1, y),
    (x + 1, y)
  ]

testm :: Matrix Int
testm = mkMatrix "2199943210\n3987894921\n9856789892\n8767896789\n9899965678\n"

s = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678\n"
