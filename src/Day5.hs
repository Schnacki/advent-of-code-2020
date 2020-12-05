module Day5 (part1, part2, solvePart1, solvePart2) where

import Data.List (sort)

binaryToDecimal :: [Char] -> Int
binaryToDecimal = foldl (\b a -> 2 * b + if a == 'B' || a == 'R' then 1 else 0) 0

solvePart1 :: [String] -> Int
solvePart1 = maximum . fmap binaryToDecimal

part1 :: FilePath -> IO ()
part1 file = print . solvePart1 . lines =<< readFile file

solvePart2 :: [String] -> Int
solvePart2 = (+ 1) . foldl1 (\a b -> if b - a > 1 then a else b) . sort . fmap binaryToDecimal

part2 :: FilePath -> IO ()
part2 file = print . solvePart2 . lines =<< readFile file