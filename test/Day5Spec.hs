module Day5Spec (spec) where

import Day5
import Test.Hspec

spec = do
  describe "Part 1:" $ do
    it "Boarding pass FBFBBFFRLR has seat id 357." $ do
      Day5.solvePart1 ["FBFBBFFRLR"] `shouldBe` 357
    it "Boarding pass BFFFBBFRRR has seat id 567." $ do
      Day5.solvePart1 ["BFFFBBFRRR"] `shouldBe` 567
    it "Boarding pass FFFBBBFRRR has seat id 119." $ do
      Day5.solvePart1 ["FFFBBBFRRR"] `shouldBe` 119
    it "Boarding pass BBFFBBFRLL has seat id 820." $ do
      Day5.solvePart1 ["BBFFBBFRLL"] `shouldBe` 820

  describe "Part 2:" $ return ()

-- hard to test