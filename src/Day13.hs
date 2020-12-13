module Day13 (part1, part2, solvePart1, parseInput) where

import Data.List.Split (splitOn)

parseInput :: String -> (Int, [Int])
parseInput str =
  let [a, b] = splitOn "\n" str
   in (read a, readList b)
  where
    readList = fmap read . filter (/= "x") . splitOn ","

solvePart1 :: Int -> [Int] -> Int
solvePart1 firstDeparture = calculateResult . minimum . map (\t -> head $ dropWhile (\(a, _) -> a < firstDeparture) (zip [0, t ..] (repeat t)))
  where
    calculateResult (departure, busId) = busId * (departure - firstDeparture)

part1 :: String -> Int
part1 = uncurry Day13.solvePart1 . parseInput

part2 :: String -> Int
part2 str = 0