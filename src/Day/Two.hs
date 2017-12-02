module Day.Two (calculateChecksum) where

calculateChecksum :: (Ord a, Num a) => [[a]] -> a
calculateChecksum = sum . fmap difOfMaxMin
  where difOfMaxMin vals = maximum vals - minimum vals
