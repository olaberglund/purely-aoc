module AOC21.Day3 (solve) where

import Data.Char (digitToInt)
import Data.Foldable (foldl')
import Data.Function (on)
import Data.List (group, maximumBy, sort, transpose)

type Bit = Char

type Bits = String

parse :: String -> [Bits]
parse = lines

solve :: String -> Int
solve = solveA
  where
    solveA :: String -> Int
    solveA = solve' gammabits epsilonbits

    solveB :: String -> Int
    solveB = solve' oxygenbits scrubberbits

solve' :: ([Bits] -> Bits) -> ([Bits] -> Bits) -> String -> Int
solve' f g = ((*) <$> r1 <*> r2) . parse
  where
    r1 = fromBinary . f
    r2 = fromBinary . g

gammabits :: [Bits] -> Bits
gammabits = map mostFrequent . transpose

mostFrequent :: Bits -> Bit
mostFrequent = head . maximumBy (compare `on` length) . group . sort

leastFrequent :: Bits -> Bit
leastFrequent = flipbit . mostFrequent

epsilonbits :: [Bits] -> Bits
epsilonbits = flipbits . gammabits

fromBinary :: Bits -> Int
fromBinary = foldl' (\xs x -> digitToInt x + 2 * xs) 0

flipbits :: Bits -> Bits
flipbits = map flipbit

flipbit :: Bit -> Bit
flipbit '0' = '1'
flipbit '1' = '0'
flipbit _ = '0'

oxygenbits :: [Bits] -> Bits
oxygenbits = findFreqBits mostFrequent

scrubberbits :: [Bits] -> Bits
scrubberbits = findFreqBits leastFrequent

findFreqBits :: (Bits -> Bit) -> [Bits] -> Bits
findFreqBits _ [b] = b
findFreqBits f bs = freqbit : findFreqBits f (map tail (filter (startsWith freqbit) bs))
  where
    freqbit :: Bit
    freqbit = (f . head . transpose) bs

    startsWith :: Bit -> Bits -> Bool
    startsWith b = (b ==) . head
