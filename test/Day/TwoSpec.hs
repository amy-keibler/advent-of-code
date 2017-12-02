module Day.TwoSpec (spec) where

import Test.Hspec

import Day.Two (calculateChecksum, calculateDivisibleChecksum)

spec :: Spec
spec = do
  describe "calculate checksum" $
    it "should sum the differences between the min and max values of each row" $
    calculateChecksum [[5, 1, 9, 5],
                       [7, 5, 3],
                       [2, 4, 6, 8]] `shouldBe` 18
  describe "calculate evenly divisible checksum" $
    it "should sum the result of the division between evenly divisible numbers in each row" $
    calculateDivisibleChecksum [[5, 9, 2, 8],
                                [9, 4, 7, 3],
                                [3, 8, 6, 5]] `shouldBe` 9
