module Day1part2 (process) where

import qualified Data.Set as Set 

readSignedInt :: String -> Int
readSignedInt ('+':int) = read int
readSignedInt ('-':int) = - read int


readFrequencyChanges :: String -> [Int]
readFrequencyChanges =
  cycle . map readSignedInt . lines


frequencies :: [Int] -> [Int]
frequencies = scanl (+) 0

firstRepetition :: [Int] -> Int
firstRepetition = go Set.empty
  where
    go set (head:tail)
      | Set.member head set = head
      | otherwise = go (Set.insert head set) tail
    
  


process :: String -> String
process  = show . firstRepetition . frequencies . readFrequencyChanges
