module Day5(part1, part2, solvePart1) where

import Data.List(sort)

row :: String -> Int
row = foldl (\ b a -> 2 * b + if (a == 'B') then 1 else 0) 0

column :: String -> Int
column = foldl (\ b a -> 2 * b + if (a == 'R') then 1 else 0) 0

solvePart1 :: String -> Int
solvePart1 boardingPass = let (r, c) = splitAt 7 boardingPass
  in 8 * row r + column c

solvePart2 :: String -> Int
solvePart2 = foldl (\ b a -> 2 * b + if (a == 'R' || a == 'B') then 1 else 0) 0

part1 :: FilePath -> IO ()
part1 file = (print . maximum . fmap solvePart1 . lines) =<< readFile file

part2 :: FilePath -> IO ()
part2 file = (print . (+1) . foldl1 (\a b -> if ((b - a) > 1) then a else b) . sort . fmap solvePart2 . lines) =<< readFile file