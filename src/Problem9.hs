module Problem9
(
    Game(..),
    newGame,
    run,
    placeMarble,
    highScore,
)
where

import ZipList
import Data.Map (Map)
import qualified Data.Map as M

data Game = Game { players :: Int, score :: Map Int Int, marbles :: ZipList Int }
    deriving Show

newGame :: Int -> Game
newGame players = Game players (M.fromList initialScore) (fromList [0])
    where initialScore = [ (p, 0) | p <- [0..(players-1)] ]

addScore :: Int -> Int -> Game -> Game
addScore player score (Game players scores list) = Game players newScore list
    where newScore = M.adjust (+score) player scores

highScore :: Game -> Int
highScore (Game _ scores _) = maximum $ M.elems scores

run :: Game -> Int -> Game
run game n = foldl placeMarble game [1..n]

placeMarble :: Game -> Int -> Game
placeMarble game@(Game p score list) m | m `mod` 23 == 0 = move1 game m
                                       | otherwise = Game p score $ ((insert m) . moveRight . moveRight) list

move1 :: Game -> Int -> Game
move1 (Game players scores list) m = addScore playerNumber scoreToAdd $ Game players scores $ remove listAfterMove
    where listAfterMove = apply 7 moveLeft list
          scoreToAdd = m + (current listAfterMove)
          playerNumber = m `mod` players

apply :: Int -> (a -> a) -> a -> a
apply x f initial = foldl (\accum i -> f accum) initial [1..x]
