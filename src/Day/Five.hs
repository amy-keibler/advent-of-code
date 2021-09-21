module Day.Five (Instructions(..)
                , mkInstructions
                , nextInstructionsState
                , numStepsToEscape
                , plusOne
                , gtThreeMinus) where

type CurrentUpdateFn = Int -> Int

plusOne :: CurrentUpdateFn
plusOne = (+1)

gtThreeMinus :: CurrentUpdateFn
gtThreeMinus n = if n >= 3 then n - 1 else n + 1

data Instructions = Instructions Int [Int] [Int] deriving (Eq, Show)

mkInstructions :: [Int] -> Maybe Instructions
mkInstructions [] = Nothing
mkInstructions (x:xs) = Just $ Instructions x [] xs

nextInstructionsState :: CurrentUpdateFn -> Instructions -> Maybe Instructions
nextInstructionsState fn (Instructions current backward forward)
  | current == 0 = Just (Instructions 1 backward forward)
  | current < 0 = if length backward < abC then Nothing else Just (Instructions (backward !! max 0 (abC - 1))
                                                                  (drop abC backward)
                                                                  (reverse (take (abC - 1) backward) ++ (fn current) : forward))
  | otherwise = if length forward < abC then Nothing else Just (Instructions (forward !! max 0 (abC - 1))
                                                                  (reverse (take (abC - 1) forward) ++ (fn current) : backward)
                                                                  (drop abC forward))
  where abC = abs current

numStepsToEscape :: CurrentUpdateFn -> Instructions -> Int
numStepsToEscape fn instructions = case nextInstructionsState fn instructions of
                                     Just nextState -> 1 + numStepsToEscape fn nextState
                                     Nothing -> 1
