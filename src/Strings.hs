module Strings
(
    dropAll
) where

dropAll :: [Int] -> String -> String
dropAll (i:is) (s1:s2:ss)
    | i == 0 = dropAll (map (\i -> i-2) is) ss
    | i > 0 = s1 : dropAll (map (\i -> i-1) (i:is)) (s2:ss)
    | otherwise = error $ show (i:is)
dropAll [] s = s


