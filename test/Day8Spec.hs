module Day8Spec (spec) where

import Day8
import Test.Hspec

computation :: Computation
computation = Computation [] 0 0 [NOP 0, ACC 1, JMP 4, ACC 3, JMP (-3), ACC (-99), ACC 1, JMP (-4), ACC 6]

spec = do
  describe "Part 1:" $ do
    it "Immediately before the program would run an instruction a second time, the value in the accumulator is 5." $ do
      Day8.solvePart1 computation `shouldBe` 5
  describe "Part 2:" $ do
    it "After the calculations the accumulator contains the value 8." $ do
      Day8.solvePart2 computation `shouldBe` 8