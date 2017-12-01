module Main where

import Data.Char (digitToInt, isDigit)

import Day.One (solveCaptcha)

main :: IO ()
main = do
  captchaInput <- readFile "inputs/DayOne.txt"
  putStrLn $ ("Day one solution: " ++) $ show $ solveCaptcha $ toCaptcha captchaInput

toCaptcha :: String -> [Int]
toCaptcha = fmap digitToInt . filter isDigit
