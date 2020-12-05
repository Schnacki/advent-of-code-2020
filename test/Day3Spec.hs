module Day3Spec (spec) where

import Day3
import Test.Hspec

area =
  [ "..##.......",
    "#...#...#..",
    ".#....#..#.",
    "..#.#...#.#",
    ".#...##..#.",
    "..#.##.....",
    ".#.#.#....#",
    ".#........#",
    "#.##...#...",
    "#...##....#",
    ".#..#...#.#"
  ]

spec = do
  describe "Part 1:" $
    it "There are 7 trees in the (3, 1) - slope!" $ do
      Day3.solvePart1 area `shouldBe` 7

  describe "Part 2:" $
    it "The trees in the (1, 1), (3, 1), (5, 1), (7, 1), (1, 2) - slopes multiply to 336!" $ do
      Day3.solvePart2 area `shouldBe` 336