module Main where

import System.Environment
import Problem1

main :: IO ()
main = do
    args <- getArgs
    let problem = head args
    input <- readFile $ "app/input/problem" ++ problem
    let solution = solveProblem "1"
    putStrLn $ show $ (`map` solution) ($ input)


solveProblem :: String -> [String -> Int]
solveProblem "1" = [lastFrequency, firstRepeatedFrequency]
