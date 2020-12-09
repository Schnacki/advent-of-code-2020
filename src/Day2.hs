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

part1 :: String -> Int
part1 = solvePart1 . parsePolicies

part2 :: String -> Int
part2 = solvePart2 . parsePolicies