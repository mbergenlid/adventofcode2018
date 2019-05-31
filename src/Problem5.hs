module Problem5
(
    polymer,
    units
) where

import qualified Data.Char as C
import Data.List

data Polymer = Polymer String
    deriving (Eq, Show)

polymer :: String -> Polymer
polymer a = Polymer (react a)

units :: Polymer -> Int
units (Polymer s) = length s

react :: String -> String
react s = case _react s of
    (Just x) -> react x
    Nothing -> s

_react :: String -> Maybe String
_react s = do
    let reacts (c1,c2) = (C.isUpper c1 && (C.toLower c1 == c2)) || (C.isLower c1 && (C.toUpper c1 == c2))
    index <- findIndex reacts $ zip s (tail s)
    return $ (take index s) ++ (drop (index+2) s)
