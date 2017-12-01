module Day.One (solveCaptcha) where

import Data.List (group)

solveCaptcha :: (Num a, Eq a) => [a] -> a
solveCaptcha captcha = sum $ fmap sumNext $ filter ((>1) . length ) $ group loopedCaptcha
  where loopedCaptcha = captcha ++ [head captcha]
        sumNext nums = sum $ tail nums
