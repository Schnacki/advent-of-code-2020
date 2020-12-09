module Day1 (part1, part2, solvePart1, solvePart2) where

import Control.Applicative ((<|>))
import Data.List (find)
import Data.Maybe (fromJust)

solvePart1 :: Int -> [Int] -> Maybe Int
solvePart1 _ [] = Nothing
solvePart1 target (x : xs) = ((* x) <$> find ((== target) . (+ x)) xs) <|> solvePart1 target xs

solvePart2 :: Int -> [Int] -> Maybe Int
solvePart2 _ [] = Nothing
solvePart2 target (x : xs) = ((* x) <$> solvePart1 (target - x) xs) <|> solvePart2 target xs

parseInput :: String -> [Int]
parseInput = fmap read . lines

part1 :: String -> Int
part1 = fromJust . solvePart1 2020 . parseInput

part2 :: String -> Int
part2 = fromJust . solvePart2 2020 . parseInput