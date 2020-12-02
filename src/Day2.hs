module Day2(solvePart1, Password(Password), part1, part2) where

import Data.Ix(inRange)
import Data.List.Split

data Password = Password (Int,Int) Char String deriving Show

passwordValid1 :: Password -> Bool
passwordValid1 (Password range letter password) = inRange range . length . filter (== letter) $ password

solvePart1 :: [Password] -> Int
solvePart1 = length . filter passwordValid1

passwordValid2 :: Password -> Bool
passwordValid2 (Password (a, b) letter password) = (password !! (a - 1) == letter) /= (password !! (b - 1) == letter) 

solvePart2 :: [Password] -> Int
solvePart2 = length . filter passwordValid2

readPassword :: String -> Password
readPassword input = let 
    (min:max:letter:_:password:_) = splitOneOf "-: " input 
    in Password (read min, read max) (head letter) password
    
part1 :: FilePath -> IO Int
part1 file = solvePart1 . fmap readPassword . lines <$> readFile file

part2 :: FilePath -> IO Int
part2 file = solvePart2 . fmap readPassword . lines <$> readFile file