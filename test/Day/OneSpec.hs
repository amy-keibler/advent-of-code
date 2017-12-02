module Day.OneSpec (spec) where

import Test.Hspec

import Day.One (solveCaptcha, solveOppositeCaptcha)

spec :: Spec
spec = do
  describe "solve captcha" $ do
    it "handles 1122" $
      solveCaptcha [1, 1, 2, 2] `shouldBe` (3 :: Integer)
    it "handles 1111" $
      solveCaptcha [1, 1, 1, 1] `shouldBe` (4 :: Integer)
    it "handles 1234" $
      solveCaptcha [1, 2, 3, 4] `shouldBe` (0 :: Integer)
    it "handles 91212129" $
      solveCaptcha [9,1,2,1,2,1,2,9] `shouldBe` (9 :: Integer)
  describe "solve opposite captcha" $ do
    it "handles 1212" $
      solveOppositeCaptcha [1,2,1,2] `shouldBe` (6 :: Integer)
    it "handles 1221" $
      solveOppositeCaptcha [1,2,2,1] `shouldBe` (0 :: Integer)
    it "handles 123425" $
      solveOppositeCaptcha [1,2,3,4,2,5] `shouldBe` (4 :: Integer)
    it "handles 123123" $
      solveOppositeCaptcha [1,2,3,1,2,3] `shouldBe` (12 :: Integer)
    it "handles 12131415" $
      solveOppositeCaptcha [1,2,1,3,1,4,1,5] `shouldBe` (4 :: Integer)
