{-# LANGUAGE OverloadedStrings #-}
module Day.SevenSpec (spec) where

import Test.Hspec

import Day.Seven (bottomProgram, Program(..), parsePrograms)

spec :: Spec
spec = do
  describe "finding the bottom program" $
    it "should find the bottom program given the input" $
      bottomProgram "pbga (66)\n\
                    \xhth (57)\n\
                    \ebii (61)\n\
                    \havc (66)\n\
                    \ktlj (57)\n\
                    \fwft (72) -> ktlj, cntj, xhth\n\
                    \qoyq (66)\n\
                    \padx (45) -> pbga, havc, qoyq\n\
                    \tknk (41) -> ugml, padx, fwft\n\
                    \jptl (61)\n\
                    \ugml (68) -> gyxo, ebii, jptl\n\
                    \gyxo (61)\n\
                    \cntj (57)\n" `shouldBe` "tknk"
  describe "parsing the programs" $
    it "should parse the input text correctly" $
     parsePrograms "pbga (66)\n\
                    \xhth (57)\n\
                    \ebii (61)\n\
                    \havc (66)\n\
                    \ktlj (57)\n\
                    \fwft (72) -> ktlj, cntj, xhth\n\
                    \qoyq (66)\n\
                    \padx (45) -> pbga, havc, qoyq\n\
                    \tknk (41) -> ugml, padx, fwft\n\
                    \jptl (61)\n\
                    \ugml (68) -> gyxo, ebii, jptl\n\
                    \gyxo (61)\n\
                    \cntj (57)\n" `shouldBe` Right [Program "pbga" 66 [],
                                                  Program "xhth" 57 [],
                                                  Program "ebii" 61 [],
                                                  Program "havc" 66 [],
                                                  Program "ktlj" 57 [],
                                                  Program "fwft" 72 ["ktlj", "cntj", "xhth"],
                                                  Program "qoyq" 66 [],
                                                  Program "padx" 45 ["pbga", "havc", "qoyq"],
                                                  Program "tknk" 41 ["ugml", "padx", "fwft"],
                                                  Program "jptl" 61 [],
                                                  Program "ugml" 68 ["gyxo", "ebii", "jptl"],
                                                  Program "gyxo" 61 [],
                                                  Program "cntj" 57 []]
