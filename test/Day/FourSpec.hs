module Day.FourSpec (spec) where

import Test.Hspec

import Day.Four (passPhraseValid, passPhraseAnagramValid)

spec :: Spec
spec = do
  describe "passphrase validation" $ do
    it "should allow aa bb cc dd ee" $
      passPhraseValid "aa bb cc dd ee" `shouldBe` True
    it "should allow aa bb cc dd aa" $
      passPhraseValid "aa bb cc dd aa" `shouldBe` False
    it "should allow aa bb cc dd aaa" $
      passPhraseValid "aa bb cc dd aaa" `shouldBe` True
  describe "passphrase anagram validation" $ do
    it "should allow abcde fghij" $
      passPhraseAnagramValid "abcde fghij" `shouldBe` True
    it "should not allow abcde xyz ecdab" $
      passPhraseAnagramValid "abcde xyz ecdab" `shouldBe` False
    it "should allow a ab abc abd abf abj" $
      passPhraseAnagramValid "a ab abc abd abf abj" `shouldBe` True
    it "should allow iiii oiii ooii oooi oooo" $
      passPhraseAnagramValid "iiii oiii ooii oooi oooo" `shouldBe` True
    it "should not allow oiii ioii iioi iiio" $
      passPhraseAnagramValid "oiii ioii iioi iiio" `shouldBe` False
