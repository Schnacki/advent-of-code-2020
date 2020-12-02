module Main where

import Day1(part1, part2)
import Day2(part1, part2)

main :: IO ()
main = do
    putStr "Day 1 - Part 1: "
    Day1.part1 "input/Day1.txt" >>= print
    putStr "Day 1 - Part 2: "
    Day1.part2 "input/Day1.txt" >>= print

    putStr "Day 2 - Part 1: "
    Day2.part1 "input/Day2.txt" >>= print
    putStr "Day 2 - Part 2: "
    Day2.part2 "input/Day2.txt" >>= print
