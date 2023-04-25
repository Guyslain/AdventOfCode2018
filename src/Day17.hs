{-# LANGUAGE OverloadedStrings #-}

module Day17 (processPart1, processPart2) where


import Text.Trifecta (parseString, Result(Success,Failure), ErrInfo, Parser)
import Text.Parser.Token (whiteSpace, integer, symbol, symbolic, textSymbol)
import Text.Parser.Combinators (some, eof, try)
import Text.Parser.Char (oneOf)
import Control.Applicative ((<|>))

import Control.Monad (when, filterM, forM, forM_)
import Data.Array.ST
import Control.Monad.ST
import Data.Array.IArray (IArray, Array, (!), bounds)
import qualified Data.Array.IArray as Arr
import Data.Array.MArray

import Data.List (find, elem)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.List as List 

type Position = (Int, Int)

sourcePosition :: Position
sourcePosition = (500,0)


parseCoordinates :: Parser [ Int ]
parseCoordinates = 
  try parseInterval <|> parseSingleInt
  where
    parseSingleInt = (\int -> [ fromIntegral int ]) <$> integer
    parseInterval = do
      mini <- integer
      _ <- symbol ".."
      maxi <- integer
      return [fromIntegral mini..fromIntegral maxi]

lineParser :: Parser [ Position ]
lineParser = do
  x <- oneOf "xy"
  _ <- textSymbol "="
  xs <- parseCoordinates
  _ <- symbol ","
  _ <- whiteSpace
  y <- oneOf "xy"
  _ <- textSymbol "="
  ys <- parseCoordinates
  if x == 'x' then
    return [ (x,y) | x <- xs, y <- ys ]
  else
    return [ (x,y) | x <- ys, y <- xs ]


parseLine :: String -> [ Position ]
parseLine line =
  case parseString lineParser mempty line of
    Success s -> s
    Failure info -> []


parseInput :: String -> [ Position ]
parseInput =
  concat
  . map parseLine
  . lines



data Cell
  = Empty
  | Clay
  | StillWater
  | BlockedLeftWater
  | BlockedRightWater
  | FlowingWater
  | Source
    deriving Eq

instance Show Cell where
  show Empty = "."
  show Clay = "#"
  show Source = "+"
  show StillWater = "~"
  show BlockedLeftWater = ">"
  show BlockedRightWater = "<"
  show flowingWater = "|"
             
here :: Position -> Position
here pos = pos

above :: Position -> Position
above (x,y) = (x,y-1)

below :: Position -> Position
below (x,y) = (x,y+1)

left :: Position -> Position
left (x,y) = (x-1,y)

right :: Position -> Position
right (x,y) = (x+1,y)


class Monad m => WorldM m where
  writeCell :: Position -> Cell -> m ()
  readCell :: Position -> m Cell
  getMinPosition :: m Position
  getMaxPosition :: m Position


isInBounds :: WorldM m => Position -> m Bool
isInBounds here = do
  mini <- getMinPosition
  maxi <- getMaxPosition
  return $ inRange (mini,maxi) here



type Pattern = [(Position -> Position, Cell -> Bool)]

is :: Cell -> Cell -> Bool
is kind cell = kind == cell

isWater :: Cell -> Bool
isWater cell =
  cell `elem` [ Source, FlowingWater, BlockedLeftWater, BlockedRightWater, StillWater ]

isSolid :: Cell -> Bool
isSolid cell =
  cell `elem` [ Clay, StillWater ]

isBlocking :: Cell -> Bool
isBlocking cell =
  cell `elem` [ Clay, BlockedLeftWater, BlockedRightWater, StillWater ]

isRightBlocking :: Cell -> Bool
isRightBlocking cell =
  cell `elem` [ Clay, BlockedRightWater, StillWater ]

isLeftBlocking :: Cell -> Bool
isLeftBlocking cell =
  cell `elem` [ Clay, BlockedLeftWater, StillWater ]

patterns :: [ (Pattern, Cell) ]
patterns =
  [ ( [(here, is Empty), (above, isWater)],
      FlowingWater)
  , ( [(here, is Empty), (left, isWater), (below . left, isSolid)],
      FlowingWater)
  , ( [(here, is Empty), (right, isWater), (below . right, isSolid)],
      FlowingWater)
  , ( [(here, is FlowingWater), (below, isSolid), (right, isBlocking)],
      BlockedRightWater)
  , ( [(here, is FlowingWater), (below, isSolid), (left, isBlocking)],
      BlockedLeftWater)
  , ( [(here, is BlockedLeftWater), (right, isRightBlocking)],
       StillWater)
  , ( [(here, is BlockedRightWater), (left, isLeftBlocking)],
       StillWater)
  , ( [(here, is BlockedRightWater), (left, is StillWater)],
      StillWater)
  , ( [(here, is BlockedLeftWater), (right, is StillWater)],
      StillWater)
  ]


neighborhood :: [ Position -> Position ]
neighborhood =
  [ here, left, right, below, above, above . right, above . left ]

getNeighborhood :: WorldM m => Position -> m [Position]
getNeighborhood position =
  filterM isInBounds positions
  where
    positions =  map ($ position) neighborhood


applyPattern :: WorldM m => Position -> (Pattern, Cell) -> m Bool
applyPattern position (pattern, newValue) = do
  satisfiesConditions <- and <$> forM pattern checkCell
  if satisfiesConditions then do
    writeCell position newValue;
    return True
  else
    return False
  where
    checkCell (direction, condition) = do
      isIn <- isInBounds $ direction position
      if isIn then condition <$> readCell (direction position)
      else return False
      


updateCell :: WorldM m => Position -> m Bool
updateCell position = do
  or <$> forM patterns (applyPattern position)


stabilize :: WorldM m => Set Position -> m ()
stabilize positions =
  if Set.null positions then return ()
  else do
    let (position, others) = Set.deleteFindMin positions 
    hasChanged <- updateCell position
    if hasChanged then do
      neighbors <- getNeighborhood position
      stabilize (foldr Set.insert others neighbors)
    else
      stabilize others
    
  
 
    
    
data World s =
  World
  { array :: STArray s Position Cell
  , yMin :: Int
  , minPosition :: Position
  , maxPosition :: Position
  }
  

initWorld :: [ Position ] -> ST s (World s)
initWorld positions = do
  world <- newArray ((minX,0), (maxX,maxY)) Empty
  forM_ positions (\pos -> writeArray world pos Clay);
  writeArray world sourcePosition Source;
  return $
    World
      { minPosition = (minX,0)
      , maxPosition = (maxX,maxY)
      , yMin = minY
      , array = world
      }
  where
    minX = (\x -> x - 1) . foldr min 500 . map fst $ positions
    maxX = (\x -> x + 1) . foldr max 500 . map fst $ positions
    minY = minimum . map snd $ positions
    maxY = foldr max 0 . map snd $ positions
    

data App s a = App { run :: World s -> ST s a }

instance Functor (App s) where
  fmap f app = App $ \ state ->
    f <$> run app state

instance Applicative (App s) where
  pure a = App $ \ state -> return a
  getF <*> getA = App $ \ state -> do
    f <- run getF state
    a <- run getA state
    return $ f a

instance Monad (App s) where
  return = pure
  getA >>= f = App $ \ state -> do
    a <- run getA state
    run (f a) state


instance WorldM (App s) where
  writeCell position content = App $ \ state ->
    writeArray (array state) position content
  readCell position = App $ \ state -> 
    readArray (array state) position
  getMinPosition = App $ \ state ->
    return $ minPosition state
  getMaxPosition  = App $ \ state ->
    return $ maxPosition state
    

getStableWorld :: String -> (Array Position Cell, [ Position ])
getStableWorld input =
  runST $ do
  state <- initWorld (parseInput input)
  run (stabilize positions) state
  finalArray <- freeze (array state)
  let allPositions = range ((xMin, yMin state), maxPosition state)
      (xMin,_) = minPosition state
  return (finalArray, allPositions)
  where
    positions = Set.singleton (below sourcePosition)
    
arrayToString :: Show e => Array Position e -> String
arrayToString array =
  unlines $
  [ concat [ show (array!(x,y)) | x <- [xMin .. xMax] ]
  | y <- [yMin .. yMax]
  ]
  where
    ((xMin,yMin),(xMax,yMax)) = bounds array

countWater :: Array Position Cell -> [ Position ] -> Int
countWater array =
  length
  . filter isWater
  . map (array!)

countStillWater :: Array Position Cell -> [ Position ] -> Int
countStillWater array =
  length
  . filter (== StillWater)
  . map (array!)


processPart0 :: String -> String
processPart0 input =
  arrayToString world
  where
    world = fst $ getStableWorld input

processPart1 :: String -> String
processPart1 input =
  show $ countWater world positions
  where
    (world, positions) = getStableWorld input

processPart2 :: String -> String
processPart2 input = 
  show $ countStillWater world positions
  where
    (world, positions) = getStableWorld input
