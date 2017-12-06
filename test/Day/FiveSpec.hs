module Day.FiveSpec (spec) where

import Test.Hspec
import Data.Maybe (fromJust)

import Day.Five

spec :: Spec
spec = do
  describe "stepping in the simulation" $ do
    it "should step from an initial state" $
      mkInstructions [0, 3, 0, 1, -3] `shouldBe` Just (Instructions 0 [] [3, 0, 1, -3])
    it "should step zero spaces forward from there and increment" $
      nextInstructionsState plusOne (Instructions 0 [] [3, 0, 1, -3]) `shouldBe` Just (Instructions 1 [] [3, 0, 1, -3])
    it "should step forward one and increment from there" $
      nextInstructionsState plusOne (Instructions 1 [] [3, 0, 1, -3]) `shouldBe` Just (Instructions 3 [2] [0, 1, -3])
    it "should step forward three and increment from there" $
      nextInstructionsState plusOne (Instructions 3 [2] [0, 1, -3]) `shouldBe` Just (Instructions (-3) [1, 0, 4, 2] [])
    it "should step back three and increment from there" $
      nextInstructionsState plusOne (Instructions (-3) [1, 0, 4, 2] []) `shouldBe` Just (Instructions 4 [2] [0, 1, -2])
    it "should step forward four and escape" $
      nextInstructionsState plusOne (Instructions 4 [2] [0, 1, -2]) `shouldBe` Nothing
  describe "number of steps to escape" $
    it "should escape in five steps" $
      numStepsToEscape plusOne (fromJust $ mkInstructions [0, 3, 0, 1, -3]) `shouldBe` 5
  describe "number of steps to escape using >=3 -> -1, else +1" $
    it "should escape in ten steps" $
      numStepsToEscape gtThreeMinus (fromJust $ mkInstructions [0, 3, 0, 1, -3]) `shouldBe` 10
