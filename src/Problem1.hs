
module Problem1
(
    lastFrequency,
    firstRepeatedFrequency,
    frequencies,
    firstRepeatedFrequencyIn,
    parse
) where

import qualified Data.Set as Set

lastFrequency :: String -> Int
lastFrequency s = (last . frequencies) $ map parse $ lines s

firstRepeatedFrequency :: String -> Int
firstRepeatedFrequency s = (firstRepeatedFrequencyIn . frequencies . cycle) $ map parse $ lines s

firstRepeatedFrequencyIn :: [Int] -> Int
firstRepeatedFrequencyIn ns = duplicate' ns Set.empty
    where duplicate' [] _ = error "No duplicate found"
          duplicate' (n:ns) s = if Set.member n s
                                then n
                                else duplicate' ns (Set.insert n s)

frequencyStrings :: [String] -> [Int]
frequencyStrings xs = frequencies numbers
    where numbers = map parse xs

parse :: String -> Int
parse ('+':xs) = read xs
parse xs = read xs

frequency :: (Num a) => [a] -> a
frequency = sum

frequencies :: (Num a) => [a] -> [a]
frequencies a = _frequencies [0] a

_frequencies :: (Num a) => [a] -> [a] -> [a]
_frequencies (f1 : fs) [] = [f1]
_frequencies (f1 : fs) (n1 : ns) = (f1) : (_frequencies (f1+n1 : f1 : fs) ns)

