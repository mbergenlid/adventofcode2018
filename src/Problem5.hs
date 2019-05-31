module Problem5
(
    problem5,
    polymer,
    shortestPolymer,
    units
) where


import Data.Strings
import qualified Data.Char as C
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

problem5 :: String -> String -> String
problem5 "1" = show . units . polymer . sTrim
problem5 "2" = show . units . shortestPolymer . sTrim

data Polymer = Polymer String
    deriving (Eq, Show)

polymer :: String -> Polymer
polymer a = Polymer $ react a

units :: Polymer -> Int
units (Polymer s) = length s

shortestPolymer :: String -> Polymer
shortestPolymer s = minimumBy (\p1 p2 -> compare (units p1) (units p2)) $ map polymerWithout allUnits
    where allUnits = ['a'..'z']
          polymerWithout c = polymer $ removeAll c s

removeAll :: Char -> String -> String
removeAll c str = _removeAll c [] str
    where _removeAll c res (s:ss) 
            | c == C.toLower s = _removeAll c res ss
            | otherwise = _removeAll c (s:res) ss
          _removeAll c res [] = res

react :: String -> String
react s = case _react s of
    (Just x) -> react x
    Nothing -> s

_react :: String -> Maybe String
_react s = do
    let reacts (c1,c2) = (C.isUpper c1 && (C.toLower c1 == c2)) || (C.isLower c1 && (C.toUpper c1 == c2))
    index <- findIndex reacts $ zip s (tail s)
    return $ (take index s) ++ react (drop (index+2) s)

tReact :: Text -> Text
tReact s = case _tReact s of
    (Just x) -> tReact x
    Nothing -> s

_tReact :: Text -> Maybe Text
_tReact s = do
    let reacts (c1,c2) = (C.isUpper c1 && (C.toLower c1 == c2)) || (C.isLower c1 && (C.toUpper c1 == c2))
    index <- findIndex reacts $ T.zip s (T.tail s)
    return $ (T.take index s) `T.append` (T.drop (index+2) s)
