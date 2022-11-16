module Main (main) where

import Day3

main :: IO ()
main = getContents >>= print . solve
