module Main where

import System.Environment
import Problem1
import Problem2
import Problem3
import Problem4
import Problem5
import Problem6

main :: IO ()
main = do
    (problem:subProblem:[]) <- getArgs
    input <- readFile $ "app/input/problem" ++ problem
    let solution = solveProblem problem subProblem
    putStrLn $ solution input


solveProblem :: String -> String -> String -> String
solveProblem "1" = problem1
solveProblem "2" = problem2
solveProblem "3" = problem3
solveProblem "4" = problem4
solveProblem "5" = problem5
solveProblem "6" = problem6
