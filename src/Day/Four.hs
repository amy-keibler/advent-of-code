module Day.Four (passPhraseValid
                , passPhraseAnagramValid) where

import Data.List (sort, nub)

passPhraseValid :: String -> Bool
passPhraseValid = allUnique . sort . words

passPhraseAnagramValid :: String -> Bool
passPhraseAnagramValid = allUnique . sort . fmap sort . words

allUnique :: [String] -> Bool
allUnique pass = length pass == (length . nub) pass
