module Problem7
(
    Dependencies,
    empty,
    new,
    add,
    delete,
    firstEmpty,
    solveFor,
    input,
    testInput
)
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

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

delete :: Step -> Dependencies -> Dependencies
delete s (Dependencies deps) = Dependencies $ M.delete s $ M.map (S.delete s) deps

solveFor :: Dependencies -> [Step]
solveFor deps
    | Problem7.null deps = []
    | otherwise, let step = firstEmpty deps = step : (solveFor (delete step deps))

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
