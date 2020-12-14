module Day14Spec (spec) where

import Day14
import Test.Hspec
import qualified Data.Map as M

input = [Day14.Mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X", Day14.Memory 8 11, Day14.Memory 7 101, Day14.Memory 8 0]

spec = do
  describe "Part 1:" $ do
    it "The memory sums up to 165." $ do
      Day14.solvePart1 input M.empty "" `shouldBe` 165