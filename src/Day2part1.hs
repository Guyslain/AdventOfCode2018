module Day2part1 where

import Data.List (sort, groupBy, any)

groupLetters :: String -> [String]
groupLetters word =
    groupBy (==)
  . sort
  $ word

countGroupOfSize :: Int -> [[String]] -> Int
countGroupOfSize n =
  length . filter hasValidGroup
  where
    hasValidGroup = any ((==) n . length)

computeCheckSum :: [String] -> Int
computeCheckSum boxIds =
  countGroupOfSize 2 preparedIds * countGroupOfSize 3 preparedIds
  where
    preparedIds = map groupLetters boxIds
    

process :: String -> String
process =
  show . computeCheckSum . lines
