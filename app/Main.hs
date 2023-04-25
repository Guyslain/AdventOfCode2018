module Main where

import System.Environment (getArgs)

import qualified Day1part1
import qualified Day1part2
import qualified Day2part1
import qualified Day2part2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14 
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25

chooseProcess :: String -> String -> String
chooseProcess arg =
  case arg of
    "day1part1" -> Day1part1.process
    "day1part2" -> Day1part2.process
    "day2part1" -> Day2part1.process
    "day2part2" -> Day2part2.process
    "day3part1" -> Day3.processPart1
    "day3part2" -> Day3.processPart2
    "day4part1" -> Day4.processPart1
    "day4part2" -> Day4.processPart2
    "day5part1" -> Day5.processPart1
    "day5part2" -> Day5.processPart2
    "day6part1" -> Day6.processPart1
    "day6part2" -> Day6.processPart2
    "day7part1" -> Day7.processPart1
    "day7part2" -> Day7.processPart2
    "day8part1" -> Day8.processPart1
    "day8part2" -> Day8.processPart2
    "day9part1" -> Day9.processPart1
    "day10part1" -> Day10.processPart1
    "day11part1" -> Day11.processPart1
    "day11part2" -> Day11.processPart2
    "day12part1" -> Day12.processPart1
    "day12part2" -> Day12.processPart2
    "day13part1" -> Day13.processPart1
    "day13part2" -> Day13.processPart2
    "day14part1" -> Day14.processPart1
    "day14part2" -> Day14.processPart2
    "day15part1" -> Day15.processPart1
    "day15part2" -> Day15.processPart2 
    "day16part1" -> Day16.processPart1
    "day16part2" -> Day16.processPart2
    "day17part1" -> Day17.processPart1
    "day17part2" -> Day17.processPart2
    "day18part1" -> Day18.processPart1
    "day18part2" -> Day18.processPart2
    "day19part1" -> Day19.processPart1
    "day19part2" -> Day19.processPart2
    "day20part1" -> Day20.processPart1
    "day20part2" -> Day20.processPart2
    "day21part1" -> Day21.processPart1
    "day21part2" -> Day21.processPart2
    "day22part1" -> Day22.processPart1
    "day22part2" -> Day22.processPart2
    "day23part1" -> Day23.processPart1
    "day23part2" -> Day23.processPart2
    "day24part1" -> Day24.processPart1
    "day24part2" -> Day24.processPart2
    "day25part1" -> Day25.processPart1
    "day25part2" -> Day25.processPart2
    
  
main :: IO ()
main = do
  input <- getContents
  [arg] <- getArgs
  putStrLn (chooseProcess arg input)
