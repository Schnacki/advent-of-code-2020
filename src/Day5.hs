module Day5(part1, part2, solvePart1) where

import Data.List(sort)

binaryToDecimal :: [Char] -> [Char] -> Int
binaryToDecimal cs = foldl (\ b a -> 2 * b + if (a `elem` cs) then 1 else 0) 0

solvePart1 :: String -> Int
solvePart1 boardingPass = let (r, c) = splitAt 7 boardingPass
  in 8 * (binaryToDecimal "B") r + (binaryToDecimal "R") c

part1 :: FilePath -> IO ()
part1 file = (print . maximum . fmap solvePart1 . lines) =<< readFile file

-- not very efficient but it does its job ... ;)
part2 :: FilePath -> IO ()
part2 file = (print . (+1) . foldl1 (\a b -> if ((b - a) > 1) then a else b) . sort . fmap (binaryToDecimal "BR") . lines) =<< readFile file