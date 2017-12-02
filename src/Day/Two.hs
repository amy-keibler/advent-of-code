module Day.Two (calculateChecksum
               , calculateDivisibleChecksum) where

import Data.Maybe (fromMaybe, catMaybes)
import Data.List (sort, tails)
import Data.List.Safe as LS

calculateChecksum :: (Ord a, Num a) => [[a]] -> a
calculateChecksum = sum . fmap difOfMaxMin
  where difOfMaxMin vals = fromMaybe 0 $ (-) <$> LS.maximum vals <*> LS.minimum vals

calculateDivisibleChecksum :: (Integral a) => [[a]] -> a
calculateDivisibleChecksum = sum . fmap wholeDivOfRow

wholeDivOfRow :: Integral a => [a] -> a
wholeDivOfRow values = sum $ catMaybes $ fmap wholeDivOfValue $ tails $ sort values

wholeDivOfValue :: Integral a => [a] -> Maybe a
wholeDivOfValue vals = quot <$> dividend <*> divisor
  where divisor = LS.head vals
        dividend = relevantDividend divisor $ LS.tail vals

relevantDividend :: Integral a => Maybe a -> Maybe [a] -> Maybe a
relevantDividend mDivisor mDividends = do
  divisor <- mDivisor
  dividends <- mDividends
  LS.head $ filter ((0==) . (`rem` divisor)) dividends
