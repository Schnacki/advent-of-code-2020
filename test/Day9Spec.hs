module Day9Spec (spec) where

import Day9
import Test.Hspec

list = [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576]

spec = do
  describe "Part 1:" $ do
    it "127 does not follow the rule." $ do
      Day9.solvePart1 5 list `shouldBe` 127