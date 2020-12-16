module Day16Spec (spec) where

import Day16
import Test.Hspec

rules = [Rule "class" [(1, 3), (5, 7)], Rule "row" [(6, 11), (33, 44)], Rule "seat" [(13, 40), (45, 50)]]

spec = do
  describe "Part 1:" $ do
    it "The sum of invalid fields is 71." $ do
      solvePart1 [7, 3, 47, 40, 4, 50, 55, 2, 20, 38, 6, 12] rules `shouldBe` 71