module Day2 (solvePart1, solvePart2, Password (Password), part1, part2) where

import Data.Ix (inRange)
import Data.List.Split

data Password = Password (Int, Int) Char String deriving (Show)

solvePart1 :: [Password] -> Int
solvePart1 = length . filter valid
  where
    valid (Password range letter password) = inRange range . length . filter (== letter) $ password

solvePart2 :: [Password] -> Int
solvePart2 = length . filter valid
  where
    valid (Password (a, b) letter password) = (password !! (a - 1) == letter) /= (password !! (b - 1) == letter)

readPasswordFile :: FilePath -> IO [Password]
readPasswordFile file = fmap readPassword . lines <$> readFile file
  where
    readPassword input =
      let (min : max : letter : _ : password : _) = splitOneOf "-: " input
       in Password (read min, read max) (head letter) password

part1 :: FilePath -> IO ()
part1 file = print . solvePart1 =<< readPasswordFile file

part2 :: FilePath -> IO ()
part2 file = print . solvePart2 =<< readPasswordFile file