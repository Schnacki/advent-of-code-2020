import Test.Hspec

import Day1Spec
import Day2Spec

main :: IO ()
main = hspec $ do
  describe "Day 1:" $ Day1Spec.spec
  describe "Day 2:" $ Day2Spec.spec