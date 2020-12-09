module Day5 (part1, part2, solvePart1, solvePart2) where

import Data.List (sort)

binaryToDecimal :: [Char] -> Int
binaryToDecimal = foldl (\b a -> 2 * b + if a == 'B' || a == 'R' then 1 else 0) 0

solvePart1 :: [String] -> Int
solvePart1 = maximum . fmap binaryToDecimal

part1 :: String -> Int
part1 = solvePart1 . lines

solvePart2 :: [String] -> Int
solvePart2 = (+ 1) . foldl1 (\a b -> if b - a > 1 then a else b) . sort . fmap binaryToDecimal

part2 :: String -> Int
part2 = solvePart2 . lines