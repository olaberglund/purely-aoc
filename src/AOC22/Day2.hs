{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module AOC22.Day2 where

data Choice = R | P | S deriving (Show, Eq, Enum, Ord, Bounded)

data Strategy = W | D | L deriving (Show)

solve :: String -> String
solve = show . sum . map play . roundsB

rounds :: String -> [[Choice]]
rounds = map (map parseA . (concat . words)) . lines

parseA :: Char -> Choice
parseA s
  | s `elem` "AX" = R
  | s `elem` "BY" = P
  | otherwise = S

play :: [Choice] -> Int
play [c1, c2] =
  point c2 + case c1 `beats` c2 of
    LT -> 6
    EQ -> 3
    GT -> 0

point :: Choice -> Int
point = (+ 1) . fromEnum

beats :: Choice -> Choice -> Ordering
beats R S = GT
beats S R = LT
beats x y = compare x y

mkChoice :: Choice -> Strategy -> Choice
mkChoice c s = case s of
  W -> beat c
  L -> lose c
  D -> c

parseOpp :: Char -> Choice
parseOpp 'A' = R
parseOpp 'B' = P
parseOpp _ = S

parseStrat :: Char -> Strategy
parseStrat 'X' = L
parseStrat 'Y' = D
parseStrat _ = W

parseB :: String -> (Choice, Strategy)
parseB [c, s] = (parseOpp c, parseStrat s)

beat :: (Enum a, Bounded a, Eq a) => a -> a
beat x = if x == maxBound then minBound else succ x

lose :: (Enum a, Bounded a, Eq a) => a -> a
lose x = if x == minBound then maxBound else pred x

roundsB :: String -> [[Choice]]
roundsB = map ((\(c, s) -> [c, mkChoice c s]) . (parseB . (concat . words))) . lines
