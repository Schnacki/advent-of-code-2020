module Day13Spec (spec) where

import Day13
import Test.Hspec

spec = do
  describe "Part 1:" $ do
    it "939 [7,13,59,31,19] results in 295." $ do
      Day13.solvePart1 939 [7,13,59,31,19] `shouldBe` 295