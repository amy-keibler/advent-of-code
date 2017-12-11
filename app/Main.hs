{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char (digitToInt, isDigit)

import Day.One (solveCaptcha, solveOppositeCaptcha)
import Day.Two (calculateChecksum, calculateDivisibleChecksum)
import Day.Three (memoryDistance, memoryNeighborsLargerThan)
import Day.Four (passPhraseValid, passPhraseAnagramValid)
import Day.Five (mkInstructions, numStepsToEscape, plusOne, gtThreeMinus)
import Day.Six (stepsToCycle, lengthOfCycle)
import Day.Seven (bottomProgram)

import Data.Maybe (fromJust)
import Data.Vector.Unboxed (fromList)
import Data.ByteString.Char8 as C (pack)

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
  instructionsInput <- (fromJust . mkInstructions) . fmap read . lines <$> readFile "inputs/DayFive.txt"
  putStrLn $ ("Day five solution (part 1): " ++) $ show $ numStepsToEscape plusOne instructionsInput
  putStrLn $ ("Day five solution (part 2): " ++) $ show $ numStepsToEscape gtThreeMinus instructionsInput
  memLayout <- (fromList . fmap read) . words <$> readFile "inputs/DaySix.tsv"
  putStrLn $ ("Day six solution (part 1): " ++) $ show $ stepsToCycle memLayout
  putStrLn $ ("Day six solution (part 2): " ++) $ show $ lengthOfCycle memLayout
  programInput <- C.pack <$> readFile "inputs/DaySeven.txt"
  putStrLn $ ("Day seven solution (part 1): " ++) $ show $ bottomProgram programInput

toCaptcha :: String -> [Int]
toCaptcha = fmap digitToInt . filter isDigit

toSpreadsheet :: String -> [[Int]]
toSpreadsheet = fmap lineToInts . lines
  where lineToInts = fmap read . words

numUniquePassphrases :: (String -> Bool) ->[String] -> Int
numUniquePassphrases fn = length . filter fn
