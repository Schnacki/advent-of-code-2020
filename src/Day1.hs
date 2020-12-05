module Day1(part1, part2, solvePart1, solvePart2) where

import Data.List(find)
import Data.Maybe(fromJust)
import Control.Applicative ((<|>))

solvePart1 :: Int -> [Int] -> Maybe Int
solvePart1 _ [] = Nothing
solvePart1 target (x:xs) = ((* x) <$> find ((== target) . (+ x)) xs) <|> solvePart1 target xs

solvePart2 :: Int -> [Int] -> Maybe Int
solvePart2 _ [] = Nothing
solvePart2 target (x:xs) = ((* x) <$> solvePart1 (target - x) xs) <|> solvePart2 target xs

readInputFile :: FilePath -> IO [Int]
readInputFile file = fmap read . lines <$> readFile file

part1 :: FilePath -> IO ()
part1 file = print . fromJust . solvePart1 2020 =<< readInputFile file

part2 :: FilePath -> IO ()
part2 file = print . fromJust . solvePart2 2020 =<< readInputFile file