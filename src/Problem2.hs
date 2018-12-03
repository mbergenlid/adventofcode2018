module Problem2
(
    countLetters,
    boxValue,
    checksum,
    diff,
    findOffByPairs,
    commonLetters
) where

import qualified Data.Map as Map

commonLetters :: String -> String -> String
commonLetters [] [] = ""
commonLetters (c1 : s1) (c2 : s2) | c1 == c2 = c1 : commonLetters s1 s2
                                  | otherwise = commonLetters s1 s2

findOffByPairs :: [String] -> [(String,String)]
findOffByPairs [] = []
findOffByPairs (id : rest) | buddies == [] = findOffByPairs rest
                           | otherwise     = (map (\x -> (id,x)) buddies) ++ findOffByPairs rest
    where buddies = filter (\x -> diff id x == 1)  rest

diff :: String -> String -> Int
diff [] [] = 0
diff (c1:s1) (c2:s2) | c1 == c2 = diff s1 s2
                     | otherwise = 1 + (diff s1 s2)

checksum :: [String] -> Int
checksum s = twos * threes
    where (twos, threes) = foldr (\(x1,x2) (y1,y2) -> (x1+y1,x2+y2)) (0,0) $ map boxValue s

boxValue :: String -> (Int, Int)
boxValue s = (twos elements, threes elements)
    where elements = Map.elems (countLetters s)
          twos   = signum . length . filter (==2)
          threes = signum . length . filter (==3)

countLetters :: String -> Map.Map Char Int
countLetters s = count' s Map.empty
    where count' [] map = map
          count' (s:ss) map = count' ss $ Map.alter increment s map
          increment Nothing = Just 1
          increment (Just n) = Just $ n+1
