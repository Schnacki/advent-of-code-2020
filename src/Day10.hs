module Day10 (part1, part2, solvePart1) where

import Data.List (sort)
import qualified Data.Map as M

parseInput :: String -> [Int]
parseInput = fmap read . lines

differences :: [Int] -> M.Map Int Int
differences [x, x'] = M.singleton (x' - x) 1
differences (x : x' : xs) =
  let map = differences (x' : xs)
   in if M.member (x' - x) map then M.adjust (+ 1) (x' - x) map else M.insert (x' - x) 1 map

solvePart1 :: [Int] -> Int
solvePart1 = (\map -> M.findWithDefault 0 1 map * (1 + M.findWithDefault 0 3 map)) . differences . sort . (:) 0

part1 :: String -> Int
part1 = solvePart1 . parseInput

part2 :: String -> Int
part2 = solvePart1 . parseInput