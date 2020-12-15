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
import Day10 (part1, part2)
import Day11 (part1, part2)
import Day12 (part1, part2)
import Day13 (part1, part2)
import Day14 (part1, part2)
import Day15 (part1, part2)

main :: IO ()
main = do
  printTop
  printTableRow "Day" "Part 1" "Part 2"
  printLine
  solveDay "1" Day1.part1 Day1.part2
  solveDay "2" Day2.part1 Day2.part2
  solveDay "3" Day3.part1 Day3.part2
  solveDay "4" Day4.part1 Day4.part2
  solveDay "5" Day5.part1 Day5.part2
  solveDay "6" Day6.part1 Day6.part2
  solveDay "7" Day7.part1 Day7.part2
  solveDay "8" Day8.part1 Day8.part2
  solveDay "9" Day9.part1 Day9.part2
  solveDay "10" Day10.part1 Day10.part2
  solveDay "11" Day11.part1 Day11.part2
  solveDay "12" Day12.part1 Day12.part2
  solveDay "13" Day13.part1 Day13.part2
  solveDay "14" Day14.part1 Day14.part2
  solveDay "15" Day15.part1 Day15.part2
  printBottom

solveDay :: (Show a, Show b) => String -> (String -> a) -> (String -> b) -> IO ()
solveDay day part1 part2 = do
  input <- readFile $ "input/Day" ++ day ++ ".txt"
  printTableRow day (show (part1 input)) (show (part2 input))

printTableRow :: String -> String -> String -> IO ()
printTableRow a b c = putStrLn $ "┃ " ++ toSize 4 a ++ "┃ " ++ toSize 16 b ++ "┃ " ++ toSize 16 c ++ "┃"
  where toSize size str = str ++ replicate (size - length str) ' '

printLine :: IO ()
printLine = putStrLn "┣━━━━━╋━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━┫"

printTop :: IO ()
printTop = putStrLn "┏━━━━━┳━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━┓"

printBottom :: IO ()
printBottom = putStrLn "┗━━━━━┻━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━┛"