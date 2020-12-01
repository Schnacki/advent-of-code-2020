module Main where

import Day1(part1, part2)

main :: IO ()
main = do
    putStrLn "Day 1 - Part 1:"
    res1_1 <- Day1.part1 "input/Day1.txt"
    print res1_1
    putStrLn "Day 1 - Part 2:"
    res1_2 <- Day1.part2 "input/Day1.txt"
    print res1_2
