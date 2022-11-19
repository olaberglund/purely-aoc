module Main (main) where

import Day4

main :: IO ()
main = getContents >>= print . solve
