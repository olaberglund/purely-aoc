module Day2 (solve) where

-- Note to self: Look into ReadP for the next days' problems

import Data.List (foldl')
import Text.Read (readMaybe)

data Direction = F | D | U

-- (horiz, depth)
type Pos = (Int, Int)

data DirectedPos = DirectedPos {pos :: !Pos, aim :: !Int}

data Command = Command !Direction !Int

solve :: String -> Int
solve s = maybe 0 moveAllB (parse s)

moveAllA :: [Command] -> Int
moveAllA = uncurry (*) . foldr move (0, 0)

moveAllB :: [Command] -> Int
moveAllB = (uncurry (*) . pos) . foldl' (flip move2) (DirectedPos (0, 0) 0)

parse :: String -> Maybe [Command]
parse = mapM (mkCommand . words) . lines

mkCommand :: [String] -> Maybe Command
mkCommand [d, n] = do
  i <- readMaybe n
  dir <- mkDirection d
  return $ Command dir i
mkCommand _ = Nothing

mkDirection :: String -> Maybe Direction
mkDirection "forward" = Just F
mkDirection "down" = Just D
mkDirection "up" = Just U
mkDirection _ = Nothing

move :: Command -> Pos -> Pos
move (Command F i) (h, d) = (h + i, d)
move (Command D i) (h, d) = (h, d + i)
move (Command U i) (h, d) = (h, d - i)

---- B

move2 :: Command -> DirectedPos -> DirectedPos
move2 (Command F i) (DirectedPos (h, d) a) = DirectedPos (h + i, d + a * i) a
move2 (Command D i) dp = dp {aim = aim dp + i}
move2 (Command U i) dp = dp {aim = aim dp - i}
