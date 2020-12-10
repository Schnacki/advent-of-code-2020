module Day10Spec (spec) where

import Day10
import Test.Hspec

spec = do
  describe "Part 1:" $ do
    it "16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4 results in 35." $ do
      Day10.solvePart1 [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4] `shouldBe` 35