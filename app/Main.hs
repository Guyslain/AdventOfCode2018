module Main where

import System.Environment (getArgs)

import qualified Day1part1
import qualified Day1part2
import qualified Day2part1
import qualified Day2part2
import qualified Day3

chooseProcess :: String -> String -> String
chooseProcess arg =
  case arg of
    "day1part1" -> Day1part1.process
    "day1part2" -> Day1part2.process
    "day2part1" -> Day2part1.process
    "day2part2" -> Day2part2.process
    "day3part1" -> Day3.processPart1
    "day3part2" -> Day3.processPart2
    
main :: IO ()
main = do
  input <- getContents
  [arg] <- getArgs
  print (chooseProcess arg input)
