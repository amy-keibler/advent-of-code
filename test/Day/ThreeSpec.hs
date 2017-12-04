module Day.ThreeSpec (spec) where

import Test.Hspec

import Day.Three (memoryDistance, memoryNeighbors)

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
  describe "memoryNeighbors" $ do
    it "should handle 1" $
      memoryNeighbors 1 `shouldBe` 1
    it "should handle 2" $
      memoryNeighbors 2 `shouldBe` 1
    it "should handle 3" $
      memoryNeighbors 3 `shouldBe` 2
    it "should handle 4" $
      memoryNeighbors 4 `shouldBe` 4
    it "should handle 5" $
      memoryNeighbors 5 `shouldBe` 5
