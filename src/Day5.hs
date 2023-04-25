module Day5 (processPart1, processPart2) where

import Data.Char (isAsciiLower, isAlpha, toLower, toUpper)
import Data.List (sort,group)

data Unit = Unit { kind :: Char, polarity :: Bool }

reacts :: Unit -> Unit -> Bool
reacts u1 u2 = kind u1 == kind u2 && polarity u1 /= polarity u2

ofChar :: Char -> Unit 
ofChar c = Unit (toLower c) (isAsciiLower c)

toChar :: Unit -> Char
toChar u
  | polarity u = kind u
  | otherwise = toUpper $ kind u 

push :: Unit -> [Unit] -> [Unit]
push u [] = [u]
push u (w:us)
  | reacts u w = us
  | otherwise = u : w : us

reduce :: [Unit] -> [Unit]
reduce = foldr push []

allKinds :: [Unit] -> String
allKinds =
    map head
  . group 
  . sort
  . map kind

findShortestReduction :: [Unit] -> Int
findShortestReduction units =
    minimum 
    . map length
    . map reduce
    . map (\k -> filter (isNotOfKind k) units)
    $ allKinds units
  where
    isNotOfKind k = (/= k) . kind

processPart1 :: String -> String
processPart1 = show . length . reduce . map ofChar . filter isAlpha

processPart2 :: String -> String
processPart2 = show . findShortestReduction . map ofChar . filter isAlpha
