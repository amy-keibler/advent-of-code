module Day.One (solveCaptcha
               , solveOppositeCaptcha) where

import Data.Maybe (catMaybes)
import Data.List (group, zipWith)

solveCaptcha :: (Num a, Eq a) => [a] -> a
solveCaptcha captcha = sum $ fmap sumNext $ filter ((>1) . length) $ group loopedCaptcha
  where loopedCaptcha = captcha ++ [head captcha]
        sumNext nums = sum $ tail nums

solveOppositeCaptcha :: (Num a, Eq a) => [a] -> a
solveOppositeCaptcha captcha = sum $ catMaybes $ fmap sumIfEqual pairedDigits
  where halfLength = length captcha `quot` 2
        pairedDigits = zipWith (\a b -> (a, captcha !! mod (b + halfLength) (length captcha))) captcha [0..]
        sumIfEqual (a, b) = if a == b then Just a else Nothing
