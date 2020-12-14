module Day14Spec (spec) where

import Day14
import Test.Hspec
import qualified Data.Map as M

input = [Mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X", Mem 8 11, Mem 7 101, Mem 8 0]

input2 = [Mask "000000000000000000000000000000X1001X", Mem 42 100, Mask "00000000000000000000000000000000X0XX", Mem 26 1]

spec = do
  describe "Part 1:" $ do
    it "The memory sums up to 165." $ do
      solvePart1 input `shouldBe` 165
  describe "Part 2:" $ do
    it "The memory sums up to 208." $ do
      solvePart2 input2 `shouldBe` 208