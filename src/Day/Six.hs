module Day.Six (reallocate
               , stepsToCycle
               , lengthOfCycle) where

import Data.Vector.Unboxed as V
import Data.Set as S
import Data.Map as M

type MemoryArea = Vector Int

reallocate :: MemoryArea -> MemoryArea
reallocate mem = V.imap updateValue mem
  where index = V.maxIndex mem
        val = mem V.! index
        len = V.length mem
        addToAll = val `quot` len
        addOneToNextN = val `rem` len
        wrappedIndex i = (i - index) `mod` len
        isWithinExtraRange i =  wrappedIndex i <= addOneToNextN && (wrappedIndex i /= 0)
        updateValue i v = (if i == index then 0 else v) + addToAll + (if isWithinExtraRange i then 1 else 0)

stepsToCycle :: MemoryArea -> Int
stepsToCycle = stepsToCycle' 0 S.empty
  where stepsToCycle' count seen mem = if S.member mem seen
          then count
          else stepsToCycle' (count + 1) (S.insert mem seen) (reallocate mem)

lengthOfCycle :: MemoryArea -> Int
lengthOfCycle = lengthOfCycle' 0 M.empty
  where lengthOfCycle' index seen mem = if M.member mem seen
          then index - (seen M.! mem)
          else lengthOfCycle' (index + 1) (M.insert mem index seen) (reallocate mem)
