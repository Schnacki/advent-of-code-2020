import Day1Spec
import Day2Spec
import Day3Spec
import Day4Spec
import Day5Spec
import Day6Spec
import Day7Spec
import Day8Spec
import Day9Spec
import Day10Spec
import Day12Spec
import Day13Spec
import Day14Spec
import Day16Spec
import Day22Spec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Day 1:" Day1Spec.spec
  describe "Day 2:" Day2Spec.spec
  describe "Day 3:" Day3Spec.spec
  describe "Day 4:" Day4Spec.spec
  describe "Day 5:" Day5Spec.spec
  describe "Day 6:" Day6Spec.spec
  describe "Day 7:" Day7Spec.spec
  describe "Day 8:" Day8Spec.spec
  describe "Day 9:" Day9Spec.spec
  describe "Day 10:" Day10Spec.spec
  describe "Day 12:" Day12Spec.spec
  describe "Day 13:" Day13Spec.spec
  describe "Day 14:" Day14Spec.spec
  describe "Day 16:" Day16Spec.spec
  describe "Day 22:" Day22Spec.spec