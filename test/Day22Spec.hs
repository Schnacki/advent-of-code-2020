module Day22Spec (spec) where

import Day22
import Test.Hspec

spec = do
  describe "Part 1:" $ do
    it "The score of the game is 306." $ do
      solvePart1 ([9, 2, 6, 3, 1], [5, 8, 4, 7, 10]) `shouldBe` 306