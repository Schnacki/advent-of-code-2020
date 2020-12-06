module Day6Spec (spec) where

import Day6
import Test.Hspec

input = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"

spec = do
  describe "Part 1:" $ do
    it "The number of questions anybody said yes to is 11." $ do
      Day6.solvePart1 input `shouldBe` 11

  describe "Part 2:" $ do
    it "The number of questions everybody said yes to is 6." $ do
      Day6.solvePart2 input `shouldBe` 6