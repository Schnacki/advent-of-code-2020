import Test.Hspec
import Test.QuickCheck

import Day1(solvePart1, solvePart2)

main :: IO ()
main = hspec $ do
  describe "Day 1" $ do
    it "The first example passes!" $ do
      Day1.solvePart1 2020 [1721, 979, 366, 299, 675, 1456] `shouldBe` (Just 514579 :: Maybe Int)
    
    it "The second example passes!" $ do
      Day1.solvePart2 2020 [1721, 979, 366, 299, 675, 1456] `shouldBe` (Just 241861950 :: Maybe Int)