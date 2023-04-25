module Day22 (processPart1, processPart2) where

import Data.Array.IArray (Array, (!))
import qualified Data.Array.IArray as Arr
import Data.Array.Unboxed (UArray)

import Data.PQueue.Min (MinQueue)
import qualified  Data.PQueue.Min as Queue
import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (sort, delete)

type Position = (Int, Int)

depth :: Int
depth = 4848

target :: Position
target = (15,700)

corner :: Position
corner = (z,z)
  where z = (fst target + snd target) * 4

data Region
  = Rocky
  | Narrow
  | Wet
  | Mouth
  | Target
  deriving Eq
             

charFromRegion :: Region -> Char
charFromRegion Rocky = '.'
charFromRegion Wet = '='
charFromRegion Narrow = '|'
charFromRegion Mouth = 'M'
charFromRegion Target = 'T'

type Erosion = Int

regionFromErosion :: Erosion -> Region
regionFromErosion n =
  case n `rem` 3 of
    0 -> Rocky
    1 -> Wet
    2 -> Narrow

type GeologicalIndex = Int

geologicalIndex ::Position -> GeologicalIndex
geologicalIndex (x,y)
  | (x,y) == (0,0) = 0
  | (x,y) == target = 0
  | y == 0 = 16807 * x
  | x == 0 = 48271 * y
  | otherwise = erosions!(x,y-1) * erosions!(x-1,y)


erosionLevel :: Position -> Erosion
erosionLevel position =
  (geologicalIndex position + depth) `rem` 20183
    

erosions :: Array Position Erosion
erosions =
  Arr.array ((0,0),corner)
    [ ((x,y), erosionLevel (x,y))
    | x <- [0..fst corner]
    , y <- [0..snd corner]
    ]


showErosions :: Position -> String
showErosions corner =
  unlines $ 
  [ [ charOf (x,y) | x <- [0..fst corner]]
  | y <- [0..snd corner]
  ]
  where
    charOf (0,0) = 'M'
    charOf pos
      | pos == target = 'T'
      | otherwise = charFromRegion . regionFromErosion $ erosions!pos


region :: Position -> Region
region pos
  | pos == (0,0) = Mouth
  | pos == target = Target
  | otherwise = regionFromErosion $ erosions!pos


totalRiskLevel :: Int
totalRiskLevel =
  sum
  . map (`rem` 3)
  $ [ erosions!(x,y) | x <- [0..fst target], y <- [0..snd target] ]

processPart1 :: String -> String
processPart1 input = show totalRiskLevel

data Tool
  = Torch
  | ClimbingGear
  | Neither
  deriving (Eq, Ord)


isUsableIn :: Tool -> Region -> Bool
Torch `isUsableIn` Wet = False
ClimbingGear `isUsableIn` Narrow = False
Neither `isUsableIn` Rocky = False
anyTool `isUsableIn` anyOtherRegion = True


data Rescuer =
  Rescuer
  { time :: Int
  , position :: Position
  , equipment :: Tool
  } deriving Eq


tieBreaker :: Ordering -> Ordering -> Ordering
tieBreaker EQ ord2 = ord2
tieBreaker ord1 _ = ord1
 

instance Ord Rescuer where
  rescuer1 `compare` rescuer2 =
    (time rescuer1 `compare` time rescuer2)
    `tieBreaker` (position rescuer1 `compare` position rescuer2)
    `tieBreaker` (equipment rescuer1 `compare` equipment rescuer2)

instance Show Rescuer where
  show (Rescuer time position equipment) =
    "Rescuer in position " ++ show position ++ " at time " ++ show time
    ++ " using " ++ equipDescr 
    where
      equipDescr = case equipment of
        Torch -> "a torch."
        ClimbingGear -> "climbing gear."
        Neither -> "no tool."
        

deltas :: Position -> [ Position ]
deltas (x,y) =
  filter (\ (x,y) -> x >= 0 && y >= 0)
  $ [ (x,y+1), (x,y-1), (x+1,y), (x-1,y) ]

isValidRescuer :: Rescuer -> Bool
isValidRescuer rescuer =
  equipment rescuer `isUsableIn` here
  where
    here = region $ position rescuer
  


possibleMoves :: Rescuer -> [ Rescuer ]
possibleMoves rescuer =
  filter isValidRescuer
  . map move
  . deltas
  $ position rescuer
  where
    move dest = Rescuer (time rescuer + 1) dest (equipment rescuer) 

possibleToolChanges :: Rescuer -> [ Rescuer ]
possibleToolChanges rescuer =
  filter isValidRescuer
  . map changeTool
  . delete (equipment rescuer)
  $ [ Torch, ClimbingGear, Neither ]
  where
    changeTool newTool =
      Rescuer (time rescuer + 7) (position rescuer) newTool
    


shortestRescueTime ::
  Map Position [ Rescuer ] -> MinQueue Rescuer -> Maybe Int
shortestRescueTime arrived inTransit =
  case Queue.minView inTransit of
    Nothing -> Nothing
    Just (rescuer, stillInTransit) ->
      if position rescuer == target && equipment rescuer == Torch then
        Just $ time rescuer
      else
        if any hasSameTool rescuersAlreadyThere then
          shortestRescueTime arrived stillInTransit
        else
          shortestRescueTime newArrived newInTransit
      where
        here = position rescuer
        rescuersAlreadyThere = Map.findWithDefault [] here arrived
        hasSameTool other = equipment rescuer == equipment other
        newArrived = Map.insert here (rescuer:rescuersAlreadyThere) arrived
        newInTransit = foldr Queue.insert inTransit newRescuers
        newRescuers = possibleMoves rescuer ++ possibleToolChanges rescuer

        
initialRescuer :: Rescuer
initialRescuer = Rescuer 0 (0,0) Torch


-- processPart2 :: String -> String
-- processPart2 input =
--   showErosions (20,20)
--   ++ "\n\n"
--   ++ ( unlines
--        . map show
--        . sort
--        . concat
--        . Map.elems
--        $ shortestRescueTime Map.empty (Queue.singleton initialRescuer)
--      )


processPart2 :: String -> String
processPart2 input =
  show $ shortestRescueTime Map.empty (Queue.singleton initialRescuer)
