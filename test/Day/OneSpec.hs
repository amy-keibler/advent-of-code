module Day.OneSpec (spec) where

import Test.Hspec

import Day.One (solveCaptcha)

spec :: Spec
spec = describe "solve captcha" $ do
  it "handles 1122" $
    solveCaptcha [1, 1, 2, 2] `shouldBe` 3
  it "handles 1111" $
    solveCaptcha [1, 1, 1, 1] `shouldBe` 4
  it "handles 1234" $
    solveCaptcha [1, 2, 3, 4] `shouldBe` 0
  it "handles 91212129" $
    solveCaptcha [9,1,2,1,2,1,2,9] `shouldBe` 9
