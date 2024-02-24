{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Arrow (second, (&&&))
import Control.Monad (guard)
import Data.Biapplicative (Biapplicative (biliftA2))
import Data.Char (isDigit)
import Data.Function (on)
import Data.List (elemIndices, intersect)
import Data.Vector hiding (all, concat, concatMap, elemIndices, findIndices, length, map, sum)
import qualified Data.Vector as V hiding (concatMap, elemIndices, findIndices, sum)
import Data.Vector.Split (chunksOf)

main :: IO ()
main = do
  ls <- lines <$> readFile "./inputs/23/input_3.txt"
  let len = length ls
      line = concat ls
      numbers = concatMap (map (V.filter (isDigit . snd)) . groupBy schemaNumber) (rows len line)
      indexedPartialNumbers = mkPartialNumber . V.map (surroundingAndNumber len line) =<< numbers

      -- -- part A
      partA = sum (map snd indexedPartialNumbers)

      -- part B
      asterisksIndices = elemIndices '*' line
      gears = do
        asterI <- asterisksIndices
        let numbersAroundAsterisk = do
              (is, n) <- indexedPartialNumbers
              let touches = not $ Prelude.null $ is `intersect` haloIndices len asterI
              guard touches
              return n
        guard (length numbersAroundAsterisk == 2)
        return numbersAroundAsterisk

  print $ sum (map Prelude.product gears)

surroundings :: Int -> Vector Char -> Int -> Vector Char
surroundings len sch i = mapMaybe (sch !?) (fromList $ haloIndices len i)

haloIndices :: Int -> Int -> [Int]
haloIndices len i = above <> same <> below
  where
    same = [i - 1, i, i + 1]
    above = map (subtract len) same
    below = map (+ len) same

schemaNumber :: (a, Char) -> (a, Char) -> Bool
schemaNumber = (&&) `on` (isDigit . snd)

rows :: Int -> [a] -> [Vector (Int, a)]
rows len = chunksOf len . indexed . fromList

mkPartialNumber :: (MonadFail m) => Vector (Vector Char, (Int, Char)) -> m ([Int], Int)
mkPartialNumber surnum
  | isPartial surnum = return $ (second read . V.foldr (biliftA2 (:) (:)) ([], "") . V.map snd) surnum
  | otherwise = fail "Not a partial number"

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c) && c /= '.'

isPartial :: Vector (Vector Char, b) -> Bool
isPartial = V.any (V.any isSymbol . fst)

surroundingAndNumber :: Int -> [Char] -> (Int, b) -> (Vector Char, (Int, b))
surroundingAndNumber len ls = surroundings len (fromList ls) . fst &&& id
