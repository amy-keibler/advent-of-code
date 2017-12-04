module Day.ThreeSpec (spec) where

import Test.Hspec

import Day.Three (memoryDistance)

spec :: Spec
spec = do
  describe "memory distance" $ do
    it "should handle square 1" $
      memoryDistance 1 `shouldBe` 0
    it "should handle square 12" $
      memoryDistance 12 `shouldBe` 3
    it "should handle square 23" $
      memoryDistance 23 `shouldBe` 2
    it "should handle square 1024" $
      memoryDistance 1024 `shouldBe` 31
