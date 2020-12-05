module Day5Spec(spec) where

import Test.Hspec
import Day5

spec = do
  describe "Part 1:" $
    it "Boarding pass FBFBBFFRLR has seat id 357." $ do
      Day5.solvePart1 area `shouldBe` 357
    it "Boarding pass BFFFBBFRRR has seat id 357." $ do
      Day5.solvePart1 area `shouldBe` 567
    it "Boarding pass FFFBBBFRRR has seat id 357." $ do
      Day5.solvePart1 area `shouldBe` 119
    it "Boarding pass BBFFBBFRLL has seat id 357." $ do
      Day5.solvePart1 area `shouldBe` 820

  describe "Part 2:" $ return ()