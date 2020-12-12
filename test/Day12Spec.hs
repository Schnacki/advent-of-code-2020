module Day12Spec (spec) where

import Day12
import Test.Hspec


spec = do
  describe "Part 1:" $ do
    it "The distance after F10, N3, F7, R90, F11 is 25." $ do
      Day12.solvePart1 [FORWARD 10, NORTH 3, FORWARD 7, RIGHT 90, FORWARD 11] `shouldBe` 25
  describe "Part 1:" $ do
    it "The distance after F10, N3, F7, R90, F11 is 286." $ do
      Day12.solvePart2 [FORWARD 10, NORTH 3, FORWARD 7, RIGHT 90, FORWARD 11] `shouldBe` 286