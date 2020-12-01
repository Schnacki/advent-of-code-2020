module Main where

import Day1(part1, part2)

main :: IO ()
main = do
    res1_1 <- Day1.part1 "../input/Day1.txt"
    print res1_1
    res1_2 <- Day1.part2 "../input/Day1.txt"
    print res1_2
