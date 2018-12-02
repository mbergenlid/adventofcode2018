module Main where

import Problem1
import Data.Maybe

main :: IO ()
main = do
    input <- readFile "app/input/problem1"
    putStrLn $ show $ frequencyStrings $ lines input
