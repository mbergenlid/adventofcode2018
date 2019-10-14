module Problem7
(
    Dependencies,
    Worker(Available),
    empty,
    new,
    add,
    delete,
    firstEmpty,
    nextStep,
    tick,
    allEmpty,
    solveFor,
    input,
    testInput,
    solvePart2
)
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Char

type Step = Char
type Dependency = (Char, Char)
newtype Dependencies = Dependencies (Map Step (Set Step))
    deriving Show

new :: [Dependency] -> Dependencies
new dep = Dependencies $ M.fromList ((initials lefts) ++ (initials rights))
    where initials deps = (map (\c -> (c, S.empty)) deps)
          lefts = map fst dep
          rights = map snd dep

null :: Dependencies -> Bool
null (Dependencies deps) = M.null deps

empty :: Dependencies
empty = Dependencies M.empty

add :: Dependency -> Dependencies -> Dependencies
add (a, b) (Dependencies map) = Dependencies $ M.insertWith S.union b (S.singleton a) map

firstEmpty :: Dependencies -> Step
firstEmpty (Dependencies deps) = _firstEmpty $ M.assocs deps
    where _firstEmpty ((step, s):xs) | S.null s = step
                                     | otherwise = _firstEmpty xs

allEmpty :: Dependencies -> [Step]
allEmpty (Dependencies deps) = _allEmpty $ M.assocs deps
    where _allEmpty ((step, s):xs) | S.null s = step : _allEmpty xs
                                   | otherwise = _allEmpty xs
          _allEmpty []             = []

delete :: Step -> Dependencies -> Dependencies
delete s (Dependencies deps) = Dependencies $ M.delete s $ M.map (S.delete s) deps

solveFor :: Dependencies -> [Step]
solveFor deps
    | Problem7.null deps = []
    | otherwise, let step = firstEmpty deps = step : (solveFor (delete step deps))


data Worker = Available | Busy Step Int
    deriving Show

step :: Worker -> Maybe Step
step (Busy s _) = Just s
step _ = Nothing

tick :: Dependencies -> [Worker] -> [Step] -> [Worker] -> (Dependencies, [Worker])
tick deps ((Busy s 0):ws) steps result    = tick (delete s deps) ws steps (Available:result)
tick deps ((Busy s c):ws) steps result    = tick deps ws steps ((Busy s (c-1)) : result)
tick deps (Available:ws) steps result     = tick deps ws steps (Available : result)
tick deps [] _ result                     = (deps, result)


pick :: Dependencies -> [Worker] -> [Step] -> [Worker] -> (Dependencies, [Worker])
pick deps (Available:ws) (s:steps) result = pick deps ws steps ((Busy s (timeForStep s)) : result)
pick deps (Available:ws) [] result        = pick deps ws [] (Available : result)
pick deps (w:ws) steps result             = pick deps ws steps (w:result)
pick deps [] _ result                     = (deps, result)

timeForStep :: Step -> Int
timeForStep step = 60 + (ord step) - (ord 'A')

nextStep :: (Dependencies, [Worker]) -> (Dependencies, [Worker])
nextStep (deps, workers) = pick depsAfterTick workersAfterTick availableSteps []
    where (depsAfterTick, workersAfterTick) = tick deps workers [] []
          availableSteps = dropWhile taken $ allEmpty depsAfterTick
          taken step = elem (Just step) takenSteps
          takenSteps = map step workersAfterTick

solvePart2 :: Dependencies -> Int -> [(Dependencies, [Worker])]
solvePart2 deps numWorkers = takeWhile notEmpty $ scanl (\state _ -> nextStep state) (deps, workers) [1..]
    where notEmpty (dep, _) = not $ Problem7.null dep
          workers = take numWorkers $ repeat Available

testInput = [
    ('C', 'A'),
    ('C', 'F'),
    ('A', 'B'),
    ('A', 'D'),
    ('B', 'E'),
    ('D', 'E'),
    ('F', 'E')]

input = [
    ('I', 'Q'),
    ('B', 'O'),
    ('J', 'M'),
    ('W', 'Y'),
    ('U', 'X'),
    ('T', 'Q'),
    ('G', 'M'),
    ('K', 'C'),
    ('F', 'Z'),
    ('D', 'A'),
    ('N', 'Y'),
    ('Y', 'Q'),
    ('Q', 'Z'),
    ('V', 'E'),
    ('A', 'X'),
    ('E', 'C'),
    ('O', 'R'),
    ('P', 'L'),
    ('H', 'R'),
    ('M', 'R'),
    ('C', 'Z'),
    ('R', 'L'),
    ('L', 'S'),
    ('S', 'X'),
    ('Z', 'X'),
    ('T', 'O'),
    ('D', 'Z'),
    ('P', 'R'),
    ('M', 'Z'),
    ('L', 'Z'),
    ('W', 'N'),
    ('Q', 'R'),
    ('P', 'C'),
    ('U', 'O'),
    ('F', 'O'),
    ('K', 'X'),
    ('G', 'K'),
    ('M', 'C'),
    ('Y', 'Z'),
    ('A', 'O'),
    ('D', 'P'),
    ('K', 'S'),
    ('I', 'E'),
    ('G', 'F'),
    ('S', 'Z'),
    ('N', 'V'),
    ('F', 'D'),
    ('A', 'Z'),
    ('F', 'X'),
    ('T', 'Y'),
    ('W', 'H'),
    ('D', 'H'),
    ('W', 'G'),
    ('J', 'X'),
    ('T', 'X'),
    ('U', 'R'),
    ('O', 'P'),
    ('L', 'X'),
    ('I', 'B'),
    ('M', 'L'),
    ('C', 'R'),
    ('R', 'X'),
    ('F', 'N'),
    ('V', 'H'),
    ('K', 'A'),
    ('W', 'O'),
    ('U', 'Q'),
    ('O', 'C'),
    ('K', 'V'),
    ('R', 'S'),
    ('E', 'S'),
    ('J', 'A'),
    ('E', 'X'),
    ('K', 'Y'),
    ('Y', 'X'),
    ('P', 'Z'),
    ('W', 'X'),
    ('Y', 'A'),
    ('V', 'X'),
    ('O', 'M'),
    ('I', 'J'),
    ('W', 'L'),
    ('I', 'G'),
    ('D', 'O'),
    ('D', 'N'),
    ('M', 'X'),
    ('I', 'R'),
    ('Y', 'M'),
    ('F', 'M'),
    ('U', 'M'),
    ('Y', 'H'),
    ('K', 'D'),
    ('N', 'O'),
    ('H', 'S'),
    ('G', 'L'),
    ('T', 'D'),
    ('J', 'N'),
    ('K', 'M'),
    ('K', 'P'),
    ('E', 'R'),
    ('N', 'H')]
