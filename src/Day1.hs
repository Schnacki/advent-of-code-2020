module Day1(part1, part2) where

import Data.List(find)

solvePart1 :: Int -> [Int] -> Maybe Int
solvePart1 _ [] = Nothing
solvePart1 expected (x:xs) = ((* x) <$> find (\i -> i + x == expected) xs) `orIfNothing` solvePart1 expected xs

solvePart2 :: Int -> [Int] -> Maybe Int
solvePart2 _ [] = Nothing
solvePart2 expected (x:xs) = ((* x) <$> solvePart1 (expected - x) xs) `orIfNothing` solvePart2 expected xs

-- There has to be an existing function for this?
orIfNothing:: Maybe a -> Maybe a -> Maybe a
orIfNothing Nothing b = b
orIfNothing (Just a) _ = Just a

readInputFile :: FilePath -> IO [Int]
readInputFile file = fmap read . lines <$> readFile file

part1 :: FilePath -> IO (Maybe Int)
part1 file = solvePart1 2020 <$> readInputFile file

part2 :: FilePath -> IO (Maybe Int)
part2 file = solvePart2 2020 <$> readInputFile file