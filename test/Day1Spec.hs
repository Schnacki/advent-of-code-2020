module Day1Spec (spec) where

import Day1
import Test.Hspec

spec = do
  describe "Part 1:" $ do
    it "1721, 979, 366, 299, 675, 1456 results in 514579!" $ do
      solvePart1 2020 [1721, 979, 366, 299, 675, 1456] `shouldBe` Just 514579
  describe "Part 2:" $ do
    it "1721, 979, 366, 299, 675, 1456 results in 241861950!" $ do
      solvePart2 2020 [1721, 979, 366, 299, 675, 1456] `shouldBe` Just 241861950