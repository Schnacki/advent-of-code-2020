module Day2 (solvePart1, solvePart2, Policy (Policy), part1, part2) where

import Data.Ix (inRange)
import Data.List.Split

data Policy = Policy (Int, Int) Char String deriving (Show)

solvePart1 :: [Policy] -> Int
solvePart1 = length . filter valid
  where
    valid (Policy range letter password) = inRange range . length . filter (== letter) $ password

solvePart2 :: [Policy] -> Int
solvePart2 = length . filter valid
  where
    valid (Policy (a, b) letter password) = (password !! (a - 1) == letter) /= (password !! (b - 1) == letter)

parsePolicies :: String -> [Policy]
parsePolicies = fmap parsePolicy . lines
  where
    parsePolicy input =
      let (min : max : letter : _ : password : _) = splitOneOf "-: " input
       in Policy (read min, read max) (head letter) password

part1 :: FilePath -> IO ()
part1 file = print . solvePart1 . parsePolicies =<< readFile file

part2 :: FilePath -> IO ()
part2 file = print . solvePart2 . parsePolicies =<< readFile file