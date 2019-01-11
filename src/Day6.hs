module Day6 (processPart1, processPart2) where


import Text.Trifecta (parseString, Result (Success), Parser)
import Text.Parser.Token (integer, symbolic)

import Data.List (any, delete)
import qualified Data.Set as Set

data Position = Position { x :: Int, y :: Int } deriving Show

instance Eq Position where
  (Position x1 y1) == (Position x2 y2) =
    x1 == x2 && y1 == y2

instance Ord Position where
  (Position x1 y1) <= (Position x2 y2) =
    x1 < x2 || (x1 == x2 && y1 <= y2)

  
positionParser :: Parser Position
positionParser = do
  x <- int
  _ <- symbolic ','
  y <- int
  return (Position x y)
  where int = fmap fromInteger integer

parseInput :: String -> [Position]
parseInput =
    map (\ (Success position) -> position)
  . map (parseString positionParser mempty)
  . lines



hasFiniteCell :: [Position] -> Position -> Bool
hasFiniteCell positions position =
  any (isEastOf position) otherPositions
  && any (isNorthOf position) otherPositions
  && any (isSouthOf position) otherPositions
  && any (isWestOf position) otherPositions
  where
    otherPositions = delete position positions
    isEastOf (Position ox oy) (Position x y) =
      abs (y - oy) <= x - ox
    isNorthOf (Position ox oy) (Position x y) =
      abs (x - ox) <= y - oy
    isWestOf (Position ox oy) (Position x y) =
      abs (y - oy) <= ox - x
    isSouthOf (Position ox oy) (Position x y) =
      abs (x - ox) <= oy - y


distance :: Position -> Position -> Int
distance (Position x1 y1) (Position x2 y2) =
  abs (x1 - x2) + abs (y1 - y2)

up (Position x y) = Position x (y+1)
down (Position x y) = Position x (y-1)
left (Position x y) = Position (x-1) y
right (Position x y) = Position (x+1) y

neighbours position =
  fmap ($ position) [up, down, left, right]

cellArea :: [Position] -> Position -> Set.Set Position
cellArea positions source =
  go Set.empty [source]
  where
    otherPositions = delete source positions
    go reached (next:queue)
      | any ((<= distance next source). distance next) otherPositions =
        go reached queue
      | otherwise = 
        go
        (Set.insert next reached)
        ( (filter (`Set.notMember` reached) $ neighbours next) ++ queue)
    go reached [] = reached


processPart1 :: String -> String
processPart1 input =
  show
  . maximum
  . map Set.size
  . map (cellArea positions)
  . filter (hasFiniteCell positions)
  $ positions
  where
    positions = parseInput input 


maxDistanceSum :: Int
maxDistanceSum = 10000

candidateMin :: [ Int ] -> Int
candidateMin coords =
  minX - delta
  where
    minX = minimum coords
    n = length coords
    cumul =  sum . map (\x -> x - minX) $ coords
    delta = (maxDistanceSum - cumul) `div` n

candidateMax :: [ Int ] -> Int
candidateMax coords =
  maxX + delta
  where
    maxX = maximum coords
    n = length coords
    cumul = sum . map (maxX -) $ coords
    delta = (maxDistanceSum - cumul) `div` n


validCoordinates :: [ Position ] -> [ (Int, Int) ]
validCoordinates positions =
  [ (x,y)
  | x <- [minX..maxX],
    y <- [minY..maxY],
    sumDistance x y < maxDistanceSum
    ]
  where
    minX = candidateMin $ map x positions
    maxX = candidateMax $ map x positions
    minY = candidateMin $ map y positions
    maxY = candidateMax $ map y positions
    sumDistance x y = sum . map (distance (Position x y)) $ positions


processPart2 :: String -> String
processPart2 =
    show
  . length
  . validCoordinates
  . parseInput
