import Day1Spec
import Day2Spec
import Day3Spec
import Day4Spec
import Day5Spec
import Day6Spec
import Day7Spec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Day 1:" Day1Spec.spec
  describe "Day 2:" Day2Spec.spec
  describe "Day 3:" Day3Spec.spec
  describe "Day 4:" Day4Spec.spec
  describe "Day 5:" Day5Spec.spec
  describe "Day 7:" Day7Spec.spec