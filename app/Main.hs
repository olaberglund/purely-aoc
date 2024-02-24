{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.Function (on)
import Data.Vector hiding (all, concat, concatMap, length, map, sum)
import qualified Data.Vector as V hiding (concatMap, sum)
import Data.Vector.Split (chunksOf)

main :: IO ()
main = do
  ls <- lines <$> readFile "./inputs/23/input_3.txt"
  let len = length ls
      numbers = concatMap (map (V.filter (isDigit . snd)) . groupBy schemaNumber) (rows len ls)
      partialNumbers :: [Int] = do
        surnums <- V.map (first $ surroundings len (fromList $ concat ls)) <$> numbers
        guard (isPartial surnums)
        return (read $ V.toList $ V.map snd surnums)
  print (sum partialNumbers)

surroundings :: Int -> Vector Char -> Int -> Vector Char
surroundings len sch i = mapMaybe (sch !?) (fromList (above <> same <> below))
  where
    same = [i - 1, i, i + 1]
    above = map (subtract len) same
    below = map (+ len) same

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c) && c /= '.'

schemaNumber :: (a, Char) -> (a, Char) -> Bool
schemaNumber = (&&) `on` (isDigit . snd)

rows :: (Foldable t) => Int -> t [a] -> [Vector (Int, a)]
rows len = chunksOf len . indexed . fromList . concat

isPartial :: Vector (Vector Char, b) -> Bool
isPartial = V.any (V.any isSymbol . fst)
