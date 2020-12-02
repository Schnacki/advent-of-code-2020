import Test.Hspec
import Test.QuickCheck

import Day1(solvePart1, solvePart2)
import Day2(solvePart1, solvePart2, Password(Password))

main :: IO ()
main = hspec $ do
  describe "Day 1" $ do
    it "The first example passes!" $ do
      Day1.solvePart1 2020 [1721, 979, 366, 299, 675, 1456] `shouldBe` Just 514579
    
    it "The second example passes!" $ do
      Day1.solvePart2 2020 [1721, 979, 366, 299, 675, 1456] `shouldBe` Just 241861950
  
  describe "Day 2" $ do
    it "The first example passes!" $ do
      Day2.solvePart1 [Password (1, 3) 'a' "abcde", Password (1, 3) 'b' "cdefg", Password (2, 9) 'c' "ccccccccc"] `shouldBe` 2

    it "The second example passes!" $ do
      Day2.solvePart2 [Password (1, 3) 'a' "abcde", Password (1, 3) 'b' "cdefg", Password (2, 9) 'c' "ccccccccc"] `shouldBe` 1