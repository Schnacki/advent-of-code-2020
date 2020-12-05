module Day2Spec (spec) where

import Day2
import Test.Hspec

spec = do
  describe "Part 1:" $ do
    it "[1-3 a: abcde] is a valid password!" $ do
      solvePart1 [Policy (1, 3) 'a' "abcde"] `shouldBe` 1
    it "[1-3 b: cdefg] is not a valid password!" $ do
      solvePart1 [Policy (1, 3) 'b' "cdefg"] `shouldBe` 0
    it "[2-9 c: ccccccccc] is a valid password!" $ do
      solvePart1 [Policy (2, 9) 'c' "ccccccccc"] `shouldBe` 1

  describe "Part 2:" $ do
    it "[1-3 a: abcde] is a valid password!" $ do
      solvePart2 [Policy (1, 3) 'a' "abcde"] `shouldBe` 1
    it "[1-3 b: cdefg] is not a valid password!" $ do
      solvePart2 [Policy (1, 3) 'b' "cdefg"] `shouldBe` 0
    it "[2-9 c: ccccccccc] is not a valid password!" $ do
      solvePart2 [Policy (2, 9) 'c' "ccccccccc"] `shouldBe` 0