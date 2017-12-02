module Day.TwoSpec (spec) where

import Test.Hspec

import Day.Two (calculateChecksum)

spec :: Spec
spec = describe "calculate checksum" $
  it "should sum the differences between the min and max values of each row" $
  calculateChecksum [[5, 1, 9, 5],
                     [7, 5, 3],
                     [2, 4, 6, 8]] `shouldBe` 18
