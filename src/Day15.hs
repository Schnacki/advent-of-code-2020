module Day15 (part1, part2, solvePart1, solvePart2) where

import qualified Data.Map as M
import Data.Maybe (maybe)

solve :: Integer -> Integer -> Integer -> (M.Map Integer Integer) -> Integer
solve limit index number map = if index == limit then number else solve limit (index + 1) (maybe 0 (\n -> index - n) (M.lookup number map)) (M.insert number index map)

solvePart1 :: Integer
solvePart1 = solve 2020 8 0 (M.fromList [(2, 1), (0, 2), (1, 3),(7,4),(4,5),(14,6),(18,7)])

solvePart2 :: Integer
solvePart2 = solve 30000000 8 0 (M.fromList [(2, 1), (0, 2), (1, 3),(7,4),(4,5),(14,6),(18,7)])

part1 :: String -> Integer
part1 str = solvePart1

part2 :: String -> Integer
part2 str = solvePart2