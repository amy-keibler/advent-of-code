{-# LANGUAGE OverloadedStrings #-}
module Day.Seven (bottomProgram
                 , parsePrograms
                 , Program(..)
                 , PName
                 , Weight) where

import Control.Applicative
import Data.ByteString
import Data.ByteString.Char8 as C
import Data.Attoparsec.ByteString.Char8 as AP
import Data.Set as S

type PName = ByteString
type Weight = Int
data Program = Program PName Weight [PName] deriving (Eq, Show)

parseProgram :: Parser Program
parseProgram = do
  pname <- AP.takeWhile (not . isSpace)
  string " ("
  weight <- decimal
  char ')'
  dependentPNames <- option [] parseDependentPNames
  endOfLine <|> endOfInput
  return $ Program pname weight dependentPNames

parseDependentPNames :: Parser [PName]
parseDependentPNames = do
  string " -> "
  ((AP.takeWhile (\c -> (c /= ',') && (c /= '\n'))) `sepBy1` (string ", "))

parsePrograms :: ByteString -> Either String [Program]
parsePrograms = parseOnly (many1 parseProgram)

bottomProgram :: ByteString -> ByteString
bottomProgram programInfo = either (C.pack . show) (bottomProgram' S.empty S.empty) $ parsePrograms programInfo

bottomProgram' :: (S.Set ByteString) -> (S.Set ByteString) -> [Program] -> ByteString
bottomProgram' potentialPrograms _ [] = if S.size potentialPrograms == 1
  then Prelude.head $ S.toList potentialPrograms
  else C.pack $ "ERROR: " ++ (show potentialPrograms)
bottomProgram' potentialPrograms seenPrograms ((Program pname _ seenNames):ps) = bottomProgram' nextPotentials nextSeen ps
  where removedPotentials = S.difference potentialPrograms (S.fromList seenNames)
        nextPotentials = if S.member pname seenPrograms
          then removedPotentials
          else S.insert pname removedPotentials
        nextSeen = S.union seenPrograms (S.fromList seenNames)
