{-# LANGUAGE TupleSections #-}
module Day.Three (memoryDistance
                 , memoryNeighbors
                 , memoryNeighborsLargerThan) where

import Data.List as L
import Data.Map as M
import Data.Maybe (fromMaybe, catMaybes)

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


memoryNeighbors :: Int -> Int
memoryNeighbors n
  | n <= 1 = 1
  | otherwise = fromMaybe 0 $ M.lookup (indexToCell n) (completeMemoryMap n)

type Cell = (Int, Int)
type NeighborMemory = M.Map Cell Int

completeMemoryMap :: Int -> NeighborMemory
completeMemoryMap n = L.foldl' addCell (M.fromList [((0,0), 1)]) $ take (n-1) cells

addCell :: NeighborMemory -> Cell -> NeighborMemory
addCell mem idxCell = M.insert idxCell cellNeighbors mem
  where cellNeighbors = sumNeighbors idxCell mem

cells :: [Cell]
cells = cellPattern 1
  where cellPattern n = ((n,) <$> [(negate n + 1).. n]) ++
          ((,n) <$> [n-1,n-2..negate n]) ++
          ((negate n,) <$> [(n - 1), (n - 2) .. (negate n)]) ++
          ((,negate n) <$> [(negate n + 1), (negate n + 2).. n]) ++
          cellPattern (n + 1)

indexToCell :: Int -> Cell
indexToCell idx = cells !! (idx - 2)

sumNeighbors :: Cell -> NeighborMemory -> Int
sumNeighbors idxCell mem = sum $ catMaybes $ (flip M.lookup) mem <$> neighborsOf idxCell

neighborsOf :: Cell -> [Cell]
neighborsOf (x, y) = [(x-1, y+1), (x, y+1), (x+1, y+1),
                      (x-1, y), (x+1, y),
                      (x-1, y-1), (x, y-1), (x+1, y-1)]

memoryNeighborsLargerThan :: Int -> Int
memoryNeighborsLargerThan n = L.head $ L.dropWhile (<n) $ memoryNeighbors <$> [1..]
