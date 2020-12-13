module Day13Spec (spec) where

import Day13
import Test.Hspec

spec = do
  describe "Part 1:" $ do
    it "939 [7,13,59,31,19] results in 295." $ do
      Day13.solvePart1 939 [7,13,59,31,19] `shouldBe` 295
  describe "Part 2:" $ do
    it "The earliest timestamp that matches the list 17,x,13,19 is 3417." $ do
      Day13.solvePart2 [(0,17),(2,13),(3,19)] `shouldBe` 3417
    it "67,7,59,61 first occurs at timestamp 754018." $ do
      Day13.solvePart2 [(0,67),(1,7),(2,59),(3,61)] `shouldBe` 754018
    it "67,x,7,59,61 first occurs at timestamp 779210." $ do
      Day13.solvePart2 [(0,67),(2,7),(3,59),(4,61)] `shouldBe` 779210
    it "67,7,x,59,61 first occurs at timestamp 1261476." $ do
      Day13.solvePart2 [(0,67),(1,7),(3,59),(4,61)] `shouldBe` 1261476