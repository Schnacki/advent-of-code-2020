module Day7Spec (spec) where

import Day7
import Test.Hspec

bags =
  [ ("light red", [(1, "bright white"), (2, "muted yellow")]),
    ("dark orange", [(3, "bright white"), (4, "muted yellow")]),
    ("bright white", [(1, "shiny gold")]),
    ("muted yellow", [(2, "shiny gold"), (9, "faded blue")]),
    ("shiny gold", [(1, "dark olive"), (2, "vibrant plum")]),
    ("dark olive", [(3, "faded blue"), (4, "dotted black")]),
    ("vibrant plum", [(5, "faded blue"), (6, "dotted black")]),
    ("faded blue", []),
    ("dotted black", [])
  ]

spec = do
  describe "Part 1:" $ do
    it "4 bag colors can eventually contain at least one shiny gold bag." $ do
      Day7.solvePart1 (Right bags) `shouldBe` Right 4
  describe "Part 2:" $ do
    it "A single shiny gold bag must contain 32 bags." $ do
      Day7.solvePart2 (Right bags) `shouldBe` Right 32