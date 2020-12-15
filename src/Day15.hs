module Day15 (part1, part2) where

import Data.List.Split (splitOn)
import qualified Data.IntMap as M
import Data.Maybe (maybe)

solve :: Int -> Int -> Int -> M.IntMap Int -> Int
solve limit index number map = if index == limit then number else solve limit (index + 1) (maybe 0 (index -) (M.lookup number map)) (M.insert number index map)

solvePart :: Int -> M.IntMap Int -> Int
solvePart limit map = solve limit (1 + M.size map) 0 map

parseInput :: String -> M.IntMap Int
parseInput = M.fromList . (flip zip) [1 ..] . fmap read . splitOn ","

part1 :: String -> Int
part1 = solvePart 2020 . parseInput

part2 :: String -> Int
part2 = solvePart 30000000 . parseInput