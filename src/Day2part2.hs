module Day2part2 where

import Control.Monad (msum)
import Data.List (find, sort)
import Data.Maybe (fromMaybe)


findDuplicate :: [String] -> Maybe String
findDuplicate =
  fmap fst 
  . find (uncurry (==))
  . consecutives
  . sort
  where
    consecutives [] = []
    consecutives xs = zip xs (tail xs)
    

extractLetter :: Int -> String -> String
extractLetter i word =
  take i word ++ drop (i+1) word 

findCommonPart :: [String] -> Maybe String
findCommonPart ids =
  msum
  . fmap findDuplicate
  . fmap (\i -> fmap (extractLetter i) ids)
  $ [0..len-1]
  where
    len = length $ head ids

process :: String -> String
process = fromMaybe "Nothing" . findCommonPart . lines

