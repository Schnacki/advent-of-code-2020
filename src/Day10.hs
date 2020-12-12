module Day10 (part1, part2, solvePart1, solvePart2) where

import Data.List (sort)
import qualified Data.Map as M

parseInput :: String -> [Int]
parseInput = fmap read . lines

differences :: [Int] -> M.Map Int Int
differences [x, x'] = M.singleton (x' - x) 1
differences (x : x' : xs) =
  let map = differences (x' : xs)
      diff = x' - x
   in if M.member diff map then M.adjust (+ 1) diff map else M.insert diff 1 map

solvePart1 :: [Int] -> Int
solvePart1 = (\map -> M.findWithDefault 0 1 map * (1 + M.findWithDefault 0 3 map)) . differences . sort . (:) 0

part1 :: String -> Int
part1 = solvePart1 . parseInput

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) (tail xs) xs

groupAdjacent :: [Int] -> [Int]
groupAdjacent [] = []
groupAdjacent (3:xs) = groupAdjacent xs
groupAdjacent (1:xs) = let (ones,rest) = span ((==) 1 ) xs in (length ones + 2):(groupAdjacent rest)

solvePart2 :: [Int] -> Int
solvePart2 = product . map toNum . groupAdjacent . diffs . sort . (:) 0
  where
    toNum 1 = 1
    toNum n = sum $ map toNum [max 1 (n-3) .. n-1]


part2 :: String -> Int
part2 = solvePart2 . parseInput