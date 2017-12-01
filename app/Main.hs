module Main where

import Data.Char (digitToInt, isDigit)

import Day.One (solveCaptcha, solveOppositeCaptcha)

main :: IO ()
main = do
  captchaInput <- toCaptcha <$> readFile "inputs/DayOne.txt"
  putStrLn $ ("Day one solution (part 1): " ++) $ show $ solveCaptcha captchaInput
  putStrLn $ ("Day one solution (part 2): " ++) $ show $ solveOppositeCaptcha captchaInput

toCaptcha :: String -> [Int]
toCaptcha = fmap digitToInt . filter isDigit
