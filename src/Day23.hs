{-# LANGUAGE OverloadedStrings #-}
module Day23 (processPart1, processPart2) where


import Text.Trifecta (parseString, Result(Success,Failure), ErrInfo, Parser)
import Text.Parser.Token (whiteSpace, integer, symbol, symbolic, textSymbol)
import Text.Parser.Combinators (some, many, eof, try, sepBy)
import Text.Parser.Char (oneOf)
import Control.Applicative ((<|>), (<*>), (*>), (<*))

import Data.List (maximumBy)

import Data.Array.IArray (Array, (!))
import qualified Data.Array.IArray as Arr


import Data.Set (Set)
import qualified Data.Set as Set


type Position = (Int, Int, Int)

distance :: Position -> Position -> Int
distance (x1,y1,z1) (x2,y2,z2) =
  abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)
  

data Nanobot =
  Nanobot
  { position :: Position
  , range :: Int
  }

type ControlArea = (Int, Int, Int, Int, Int, Int, Int, Int)

controlAreaFromBot :: Nanobot -> ControlArea
controlAreaFromBot bot =
  ( x+y+z-r, x+y+z+r,
    x+y-z-r, x+y-z+r,
    x-y+z-r, x-y+z+r,
   -x+y+z-r,-x+y+z+r
  )
  where
    (x,y,z) = position bot
    r = range bot

intersect :: ControlArea -> ControlArea -> ControlArea
intersect (d1,d2,d3,d4,d5,d6,d7,d8) (e1,e2,e3,e4,e5,e6,e7,e8) =
  ( max d1 e1, min d2 e2,
    max d3 e3, min d4 e4,
    max d5 e5, min d6 e6,
    max d7 e7, min d8 e8
  )


smallestPoint :: ControlArea -> Position
smallestPoint (d1,d2,d3,d4,d5,d6,d7,d8) =
  foldr1 min points
  where
    min (x1,y1,z1) (x2,y2,z2) =
      if abs x1 + abs y1 + abs z1 <= abs x2 + abs y2 + abs z2 then (x1,y1,z1)
      else (x2,y2,z2)
    points =
      [ (x,y,z)
      | a <- [d1..d2]
      , b <- [d3..d4]
      , c <- [d5..d6]
      , a `mod` 2 == b `mod` 2 && a `mod` 2 == c `mod` 2
      , let (x,y,z) = ( (b+c) `div` 2, (a-b) `div` 2, (a-c) `div` 2)
      , d7 <= y+z-x && y+z-x <= d8
      ]

isEmpty :: ControlArea -> Bool
isEmpty (d1,d2,d3,d4,d5,d6,d7,d8) =
  d2 > d1 || d3 > d4 || d5 > d6 || d7 > d8

  

compareByRange :: Nanobot -> Nanobot -> Ordering
compareByRange bot1 bot2 = compare (range bot1) (range bot2)


isCoveredBy :: Position -> Nanobot -> Bool
pos `isCoveredBy` bot =
  distance pos (position bot) <= range bot

isInRangeOf :: Nanobot -> Nanobot -> Bool
bot1 `isInRangeOf` bot2 =
  distance (position bot1) (position bot2) <= range bot2


haveOverlappingRange :: Nanobot -> Nanobot -> Bool
haveOverlappingRange bot1 bot2 =
  distance (position bot1) (position bot2) < range bot1 + range bot2
  

findStrongest :: [ Nanobot ] -> Nanobot
findStrongest = maximumBy compareByRange



parseLine :: String -> Nanobot
parseLine line =
  case parseString parser mempty line of
    Success bot -> bot
  where
    parser = do
      textSymbol "pos"
      symbol "="
      symbol "<"
      x <- fromIntegral <$> integer
      symbol ","
      y <- fromIntegral <$> integer
      symbol ","
      z <- fromIntegral <$> integer
      symbol ">"
      symbol ","
      whiteSpace
      textSymbol "r"
      symbol "="
      r <- fromIntegral <$> integer
      return $ Nanobot (x,y,z) r
      

readBots :: String -> [ Nanobot ]
readBots = map parseLine . lines

processPart1 :: String -> String
processPart1 input =
  show
  . length
  . filter (`isInRangeOf` strongest)
  $ bots
  where
    bots = readBots input
    strongest = findStrongest bots


mkBots :: [ Nanobot ] -> Array Int Nanobot
mkBots bots =
  Arr.listArray (1, length bots) bots
  

type Graph = Array (Int,Int) Bool

mkAdjacencies :: Array Int Nanobot -> Graph
mkAdjacencies bots =
  Arr.array
    ((min,min), (max,max))
    [ ((u,v), haveOverlappingRange (bots!u) (bots!v))
    | u <- [min..max], v <- [min..max]
    ]
  where
    (min,max) = Arr.bounds bots




greedyClique :: Graph -> Set Int
greedyClique areAdjacent =
  go Set.empty min
  where
    ((min,_), (max,_)) = Arr.bounds areAdjacent
    go clique nextVertex
      | nextVertex > max = clique
      | all (\v -> areAdjacent!(v,nextVertex)) clique =
        go (Set.insert nextVertex clique) (nextVertex+1)
      | otherwise = go clique (nextVertex+1)

        

processPart2 :: String -> String
processPart2 input =
  show $ x + y + z
  where
    (x,y,z) = smallestPoint intersection
    intersection =
      foldr1 intersect . map controlAreaFromBot . map (\i -> bots!i) $ Set.toList clique
    clique = greedyClique adjacencies
    bots = mkBots $ readBots input
    adjacencies = mkAdjacencies bots
