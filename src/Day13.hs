module Day13 (part1, part2, solvePart1, parseInput) where

import Data.List.Split (splitOn)

import Math.NumberTheory.Moduli.Chinese

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

{-
Equations to solve:

t +  0 mod 17  = 0 <=> t mod 17  = 0
t + 11 mod 37  = 0 <=> t mod 37  = 26
t + 17 mod 439 = 0 <=> t mod 439 = 422
t + 19 mod 29  = 0 <=> t mod 29  = 10
t + 30 mod 13  = 0 <=> t mod 13  = 9
t + 40 mod 23  = 0 <=> t mod 40  = 6
t + 48 mod 787 = 0 <=> t mod 48  = 739
t + 58 mod 41  = 0 <=> t mod 41  = 24
7 + 67 mod 19  = 0 <=> t mod 19  = 9

-}

part2 :: String -> Integer
part2 str = let Just res = chineseRemainder [(0,17),(26,37),(422,439),(10,29),(9,13),(6,23),(739,787),(24,41),(9,19)] in res
