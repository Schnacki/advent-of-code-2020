module Day10Spec (spec) where

import Day10
import Test.Hspec

spec = do
  describe "Part 1:" $ do
    it "16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4 results in 35." $ do
      Day10.solvePart1 [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4] `shouldBe` 35
    it "28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3 results in 220." $ do
      Day10.solvePart1 [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3] `shouldBe` 220