module Day15 (part1, part2) where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (maybe)

solve :: Integer -> Integer -> Integer -> M.Map Integer Integer -> Integer
solve limit index number map = if index == limit then number else solve limit (index + 1) (maybe 0 (index -) (M.lookup number map)) (M.insert number index map)

solvePart :: Integer -> M.Map Integer Integer -> Integer
solvePart limit map = solve limit ((toInteger $ M.size map) + 1) 0 map

parseInput :: String -> M.Map Integer Integer
parseInput = M.fromList . (flip zip) [1 ..] . fmap read . splitOn ","

part1 :: String -> Integer
part1 = solvePart 2020 . parseInput

part2 :: String -> Integer
part2 = solvePart 30000000 . parseInput