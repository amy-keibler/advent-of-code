module Main where

import Data.Char (digitToInt, isDigit)

import Day.One (solveCaptcha, solveOppositeCaptcha)
import Day.Two (calculateChecksum, calculateDivisibleChecksum)
import Day.Three (memoryDistance, memoryNeighborsLargerThan)
import Day.Four (passPhraseValid, passPhraseAnagramValid)

main :: IO ()
main = do
  captchaInput <- toCaptcha <$> readFile "inputs/DayOne.txt"
  putStrLn $ ("Day one solution (part 1): " ++) $ show $ solveCaptcha captchaInput
  putStrLn $ ("Day one solution (part 2): " ++) $ show $ solveOppositeCaptcha captchaInput
  checksumInput <- toSpreadsheet <$> readFile "inputs/DayTwo.tsv"
  putStrLn $ ("Day two solution (part 1): " ++) $ show $ calculateChecksum checksumInput
  putStrLn $ ("Day two solution (part 2): " ++) $ show $ calculateDivisibleChecksum checksumInput
  putStrLn $ ("Day three solution (part 1): " ++) $ show $ memoryDistance 325489
  putStrLn $ ("Day three solution (part 2): " ++) $ show $ memoryNeighborsLargerThan 325489
  passphraseInput <- lines <$> readFile "inputs/DayFour.txt"
  putStrLn $ ("Day four solution (part 1): " ++) $ show $ numUniquePassphrases passPhraseValid passphraseInput
  putStrLn $ ("Day four solution (part 2): " ++) $ show $ numUniquePassphrases passPhraseAnagramValid passphraseInput

toCaptcha :: String -> [Int]
toCaptcha = fmap digitToInt . filter isDigit

toSpreadsheet :: String -> [[Int]]
toSpreadsheet = fmap lineToInts . lines
  where lineToInts = fmap read . words

numUniquePassphrases :: (String -> Bool) ->[String] -> Int
numUniquePassphrases fn = length . filter fn
