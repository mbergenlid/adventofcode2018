
module Problem1
(
    frequency,
    frequencyStrings
) where

frequencyStrings :: [String] -> Int
frequencyStrings xs = frequency numbers
    where numbers = map parse xs

parse :: String -> Int
parse ('+':xs) = read xs
parse xs = read xs

frequency :: (Num a) => [a] -> a
frequency = sum

