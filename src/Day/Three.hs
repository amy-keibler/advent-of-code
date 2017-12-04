module Day.Three (memoryDistance) where

{- This Memory Layout has an interesting property, where the cell distances
can be described as an infinite list via the following:
           <- 8
6 5 4 3 4 5 6 7
5 4 3 2 3 4 5 6
4 3 2 1 2 3 4 5
3 2 1 0 1 2 3 4
4 3 2 1 2 3 4 5
5 4 3 2 3 4 5 6
6 5 4 3 4 5 6 7

[0] ++ 4x[1,2] ++ 4x[3,2,3,4] ++ 4x[5,4,3,4,5,6] ++ 4x[7,6,5,,4,5,6,7,8]
                    [3,2,..3+1]    [5,4..3..5+1]      [7,6..4..8]
-}

memoryLayout :: [Int]
memoryLayout = [0] ++ repeat4x [1,2] ++ repeatingPattern 2 4

repeat4x :: [n] -> [n]
repeat4x vals = concat $ replicate 4 vals

repeatingPattern :: Int -> Int  -> [Int]
repeatingPattern low high = repeat4x p ++ repeatingPattern (low + 1) (high + 2)
  where p = [high - 1, high - 2 .. low] ++ [low + 1 .. high]

memoryDistance :: Int -> Int
memoryDistance n
  | n <= 1 = 0
  | otherwise = memoryLayout !! (n - 1)
