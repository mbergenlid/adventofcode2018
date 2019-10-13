module Problem6
(
    problem6,
    largestArea,
    boundedRect,
    Rect(..),
    manhattanDistance,
    locationsIn,
    areas,
    strictMinimumBy,
    input,
    debug,
    debug2,
    labels,
    allSafeAreas
)
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.List

problem6 :: String -> String -> String
problem6 "1" _ = show (largestArea input)

type Location = (Int,Int)
data Rect = Rect { topLeft :: Location, width :: Int, height :: Int }
    deriving (Show, Eq)

largestArea :: [(Int,Int)] -> Int
largestArea locations = maximum $ filter notInfinite $ M.elems $ areas locations
    where notInfinite n = n >= 0

areas :: [Location] -> Map Location Int
areas locations = _buildMap allLocations empty
    where allLocations = locationsIn $ boundedRect locations
          empty = M.fromList $ zip locations $ repeat 0

_buildMap :: [Location] -> Map Location Int -> Map Location Int
_buildMap (l:ls) map = _buildMap ls adjust
    where key = strictMinimumBy compareManhattanDistance (M.keys map)
          compareManhattanDistance loc1 loc2 = compare (manhattanDistance loc1 l) (manhattanDistance loc2 l)
          adjust = case key of
            (Just k) -> M.adjust (+1) k map
            Nothing -> map
_buildMap [] map = map

debug :: [Location] -> String
debug locations = [ c | y <- [y..y+h], c <- (generateRow y) ++ "\n" ]
    where locationsWithLabels = M.fromList $ zip locations (['A'..'Z'] ++ ['a'..'z'])
          (Rect (x,y) w h) = boundedRect locations
          generateRow y = [ label (x,y) | x <- [x..x+w] ]
          label l = case M.lookup l locationsWithLabels of
            (Just x) -> x
            Nothing -> '.'

debug2 :: Int -> [Location] -> String
debug2 limit locations = [ c | y <- [y..y+h], c <- (generateRow y) ++ "\n" ]
    where locationsWithLabels = M.fromList $ zip locations (['A'..'Z'] ++ ['a'..'z'])
          (Rect (x,y) w h) = boundedRect locations
          generateRow y = [ label (x,y) | x <- [x..x+w] ]
          label l = if elem l (allSafeAreas limit locations)
                    then '#'
                    else
                        case M.lookup l locationsWithLabels of
                            (Just x) -> x
                            Nothing -> '.'

allSafeAreas limit locations = map fst $ filter (\(_, v) -> v < limit) $ map (\l -> (l, value l)) allLocations
    where allLocations = locationsIn $ boundedRect locations
          value loc = sum $ map (manhattanDistance loc) locations

labels :: [Location] -> Map Location Char
labels locations = M.fromList $ zip locations (['A'..'Z'] ++ ['a'..'z'])

debugIO :: [Location] -> IO ()
debugIO locations = putStrLn $ debug locations

strictMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
strictMinimumBy f (x:xs) = _strictMin xs x True
    where _strictMin (x:xs) min unique
                | f x min == LT = _strictMin xs x True
                | f x min == EQ = _strictMin xs x False
                | otherwise = _strictMin xs min unique
          _strictMin [] min True = Just min
          _strictMin [] min False = Nothing


boundedRect :: [Location] -> Rect
boundedRect locations = Rect (topLeftX, topLeftY) width height
    where topLeftX = minimum $ map fst locations
          topLeftY = minimum $ map snd locations
          bottomRightX = maximum $ map fst locations
          bottomRightY = maximum $ map snd locations
          width = bottomRightX - topLeftX
          height = bottomRightY - topLeftY

manhattanDistance :: Location -> Location -> Int
manhattanDistance (x1, y1) (x2, y2) = (abs (y2 - y1)) + (abs (x2 - x1))

locationsIn :: Rect -> [Location]
locationsIn (Rect (x,y) w h) = [ (x,y) | x <- [x..x+w], y <- [y..y+h]]

input = [
    (192, 212),
    (294, 73),
    (153, 248),
    (238, 54),
    (354, 207),
    (269, 256),
    (155, 329),
    (132, 308),
    (211, 173),
    (261, 241),
    (300, 218),
    (143, 43),
    (226, 348),
    (148, 349),
    (114, 78),
    (77, 327),
    (140, 327),
    (202, 346),
    (174, 115),
    (86, 198),
    (132, 152),
    (167, 184),
    (146, 259),
    (277, 288),
    (330, 199),
    (98, 332),
    (290, 186),
    (322, 120),
    (295, 355),
    (346, 260),
    (305, 190),
    (294, 82),
    (156, 159),
    (114, 263),
    (340, 220),
    (353, 207),
    (220, 219),
    (152, 122),
    (223, 319),
    (236, 243),
    (358, 348),
    (174, 116),
    (306, 74),
    (70, 264),
    (352, 351),
    (194, 214),
    (153, 322),
    (225, 99),
    (237, 331),
    (279, 208)]
