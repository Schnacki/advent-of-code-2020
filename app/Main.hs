module Main where

import Day1(part1, part2)
import Day2(part1, part2)
import Day3(part1, part2)
import Day4(part1, part2)
import Day5(part1)

main :: IO ()
main = do
    putStr "Day 1 - Part 1: "
    Day1.part1 "input/Day1.txt"
    putStr "Day 1 - Part 2: "
    Day1.part2 "input/Day1.txt"

    putStr "Day 2 - Part 1: "
    Day2.part1 "input/Day2.txt"
    putStr "Day 2 - Part 2: "
    Day2.part2 "input/Day2.txt"

    putStr "Day 3 - Part 1: "
    Day3.part1 "input/Day3.txt"
    putStr "Day 3 - Part 2: "
    Day3.part2 "input/Day3.txt"

    putStr "Day 4 - Part 1: "
    Day4.part1 "input/Day4.txt"
    putStr "Day 4 - Part 2: "
    Day4.part2 "input/Day4.txt"

    putStr "Day 5 - Part 1: "
    Day5.part1 "input/Day5.txt"
