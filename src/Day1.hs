module Day1(
  part1,
  part2,
  ) where

import Data.List(find)

solvePart1 :: Int -> [Int] -> Maybe Int
solvePart1 _ [] = Nothing
solvePart1 res (x:xs) = case find (\i -> i + x == res) xs of
  Just f -> Just $ f * x
  Nothing -> solvePart1 res xs

solvePart2 :: Int -> [Int] -> Maybe Int
solvePart2 _ [] = Nothing
solvePart2 res (x:xs) = case solvePart1 (res - x) xs of
  Just f -> Just $ f * x
  Nothing -> solvePart2 res xs

part1 :: FilePath -> IO (Maybe Int)
part1 file = solvePart1 2020 . (fmap read . lines) <$> readFile file

part2 :: FilePath -> IO (Maybe Int)
part2 file = solvePart2 2020 . (fmap read . lines) <$> readFile file