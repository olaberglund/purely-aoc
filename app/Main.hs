module Main where

import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.Function (on)
import Data.Vector hiding (all, concat, length, map, sum)
import qualified Data.Vector as V hiding (sum)
import Data.Vector.Split (chunksOf)

main :: IO ()
main = do
  ls <- lines <$> readFile "./inputs/23/input_3.txt"
  let len = length ls
      numbers = Prelude.concatMap (map (V.filter (isDigit . snd)) . groupBy schemaNumber) (rows len ls)
      surNums = (map $ V.map $ first $ surroundings len (fromList $ concat ls)) numbers
      partialNumbers = map (V.map snd) (Prelude.filter (V.any $ V.any isSymbol . fst) surNums)
      total = sum $ map (read . V.toList) partialNumbers
  print total

surroundings :: Int -> Vector Char -> Int -> Vector Char
surroundings len sch i = mapMaybe (sch !?) (fromList (above <> same <> below))
  where
    above = [i - len - 1, i - len, i - len + 1]
    same = [i - 1, i + 1]
    below = [i + len, i + len + 1, i + len - 1]

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c) && c /= '.'

schemaNumber :: (a, Char) -> (a, Char) -> Bool
schemaNumber = (&&) `on` (isDigit . snd)

rows :: (Foldable t) => Int -> t [a] -> [Vector (Int, a)]
rows len = chunksOf len . indexed . fromList . concat
