module Day6 (part1, part2, parseInput) where
import Data.List (nub, intersect)
import Data.List.Split (splitWhen)

parseInput :: String -> [[String]]
parseInput =  splitWhen (== "") . lines

solvePart1 :: String -> Int
solvePart1 = sum . (fmap (length . nub . foldr1 (\w s -> w ++ s))) . parseInput

part1 :: FilePath -> IO ()
part1 file = print . solvePart1 =<< readFile file

solvePart2 :: String -> Int
solvePart2 = sum . (fmap (length . foldl1 (intersect))) . parseInput

part2 :: FilePath -> IO ()
part2 file = print . solvePart2 =<< readFile file