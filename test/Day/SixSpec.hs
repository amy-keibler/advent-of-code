module Day.SixSpec (spec) where

import Test.Hspec

import Data.Vector.Unboxed

import Day.Six

spec :: Spec
spec = do
  describe "stepping the memory reallocation" $ do
    it "should step from the initial state" $
      reallocate (fromList [0, 2, 7, 0]) `shouldBe` fromList [2, 4, 1, 2]
    it "should proceed from [2, 4, 1, 2]" $
      reallocate (fromList [2, 4, 1, 2]) `shouldBe` fromList [3, 1, 2, 3]
    it "should proceed from [3, 1, 2, 3]" $
      reallocate (fromList [3, 1, 2, 3]) `shouldBe` fromList [0, 2, 3, 4]
    it "should proceed from [0, 2, 3, 4]" $
      reallocate (fromList [0, 2, 3, 4]) `shouldBe` fromList [1, 3, 4, 1]
    it "should proceed from [1, 3, 4, 1]" $
      reallocate (fromList [1, 3, 4, 1]) `shouldBe` fromList [2, 4, 1, 2]
  describe "cycle detection" $
    it "should find the cycle from [0, 2, 7, 0]" $
      stepsToCycle (fromList [0, 2, 7, 0]) `shouldBe` 5
  describe "cycle length detection" $
    it "should find the length of the cycle from [0, 2, 7, 0]" $
      lengthOfCycle (fromList [0, 2, 7, 0]) `shouldBe` 4
