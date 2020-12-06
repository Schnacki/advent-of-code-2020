module Day6 (part1, part2, solvePart1, solvePart2) where

import Data.List (intersect, nub)
import Data.List.Split (splitWhen)

solvePart1 :: String -> Int
solvePart1 = sum . fmap (length . nub . foldr1 (++)) . splitWhen (== "") . lines

part1 :: FilePath -> IO ()
part1 file = print . solvePart1 =<< readFile file

solvePart2 :: String -> Int
solvePart2 = sum . fmap (length . foldl1 intersect) . splitWhen (== "") . lines

part2 :: FilePath -> IO ()
part2 file = print . solvePart2 =<< readFile file