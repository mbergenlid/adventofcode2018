module Main where

import System.Environment
import Data.Strings
import Problem1
import Problem2
import Problem3
import Problem4
import Problem5

main :: IO ()
main = do
    args <- getArgs
    let problem = head args
    input <- readFile $ "app/input/problem" ++ problem
    let solution = solveProblem problem
    putStrLn $ show $ (`map` solution) ($ input)


solveProblem :: String -> [String -> String]
solveProblem "1" = [show . lastFrequency, show . firstRepeatedFrequency]
solveProblem "2" = [show . checksum . lines, show . (map (\(a,b) -> commonLetters a b)) . findOffByPairs . lines]
solveProblem "3" = [show . squaresInTwoOrMore . (map read) . lines, show . doesNotOverlapAnyOther . (map read) . lines]
solveProblem "4" = [\s -> show findBestGuard, \s -> show strategy2 ]
solveProblem "5" = [show . units . polymer . sTrim]
