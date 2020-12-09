module Main where

import Day1 (part1, part2)
import Day2 (part1, part2)
import Day3 (part1, part2)
import Day4 (part1, part2)
import Day5 (part1, part2)
import Day6 (part1, part2)
import Day7 (part1, part2)
import Day8 (part1, part2)
import Day9 (part1, part2)

main :: IO ()
main = do
  solveDay 1 Day1.part1 Day1.part2
  solveDay 2 Day2.part1 Day2.part2
  solveDay 3 Day3.part1 Day3.part2
  solveDay 4 Day4.part1 Day4.part2
  solveDay 5 Day5.part1 Day5.part2
  solveDay 6 Day6.part1 Day6.part2
  solveDay 7 Day7.part1 Day7.part2
  solveDay 8 Day8.part1 Day8.part2
  solveDay 9 Day9.part1 Day9.part2

solveDay :: Int -> (FilePath -> IO ()) -> (FilePath -> IO ()) -> IO ()
solveDay day part1 part2 = do
  putStr $ "Day " ++ show day ++ " - Part 1: "
  part1 $ "input/Day" ++ show day ++ ".txt"
  putStr $ "Day " ++ show day ++ " - Part 2: "
  part2 $ "input/Day" ++ show day ++ ".txt"
  putStrLn $ replicate 30 '-'