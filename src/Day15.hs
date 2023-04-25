{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Day15 (processPart1, processPart2) where

import Prelude hiding (round)

import Data.Array.ST (runSTArray, MArray, STArray)
import qualified Data.Array.MArray as Arr
import qualified Data.Array.Base as ArrBase
import Data.Ix

import Control.Monad.ST
import Control.Monad
import Control.Monad.State.Lazy 

import Data.List (nub, groupBy, sort, sortBy, sortOn, intercalate)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Array (Array)
import Data.Set (Set)
import qualified Data.Set as Set


data Position = Position (Int,Int) deriving Eq

instance Show Position where
  show (Position (x,y)) = show (x,y)

instance Ord Position where  
  compare (Position (x1,y1)) (Position (x2,y2)) =
    if y1 == y2 then compare x1 x2 else compare y1 y2
                                      
instance Ix Position where
  range (Position p1, Position p2) = map Position (range (p1, p2))
  inRange (Position l, Position u) (Position p) = inRange (l,u) p
  index (Position l, Position u) (Position p)= index (l,u) p
  
removeDuplicate :: Ord a => [a] -> [a]
removeDuplicate = nub . sort

data Race
  = Elf
  | Gobelin
  deriving Eq

instance Show Race where
  show Elf = "elf"
  show Gobelin = "gobelin"

enemy :: Race -> Race
enemy Elf = Gobelin
enemy Gobelin = Elf

data Monster
  = Monster
    { race :: Race
    , hitPoints :: Int
    , attackPower :: Int
    }
    deriving Eq

instance Ord Monster where
  compare m1 m2 = compare (hitPoints m1) (hitPoints m2)

instance Show Monster where
  show (Monster race hp power) =
    show race ++ "(" ++ show hp ++ "," ++ show power ++ ")"


    
data Cell
  = Empty
  | Wall
  | Occupied Monster
  deriving Eq
           

isElf :: Cell -> Bool
isElf (Occupied m) = race m == Elf
isElf cell = False

isGobelin :: Cell -> Bool
isGobelin (Occupied m) = race m == Gobelin
isGobelin cell = False

isEnemyOf :: Race -> Cell -> Bool
isEnemyOf Elf = isGobelin
isEnemyOf Gobelin = isElf

isEmpty :: Cell -> Bool
isEmpty cell = cell == Empty

cellToMonster :: Cell -> Maybe Monster
cellToMonster (Occupied m) = Just m
cellToMonster cell = Nothing


cellToChar :: Cell -> Char
cellToChar Empty = '.'
cellToChar Wall = '#'
cellToChar (Occupied p)
  | race p == Elf = 'E'
  | race p == Gobelin = 'G'
  | otherwise = '?'    


data Distance
  = Infinite
  | Finite Int deriving (Eq, Show)

incr :: Distance -> Distance
incr Infinite = Infinite
incr (Finite n) = Finite (n+1)

decr :: Distance -> Distance
decr Infinite = Infinite
decr (Finite n) = Finite (n-1)

isFinite :: Distance -> Bool
isFinite Infinite = False
isFinite (Finite _) = True

distToChar :: Distance -> Char
distToChar Infinite = ' '
distToChar (Finite n)
  | n < 10 = head (show n)
  | otherwise = '+'
    
instance Ord Distance where
  compare Infinite Infinite = EQ
  compare Infinite (Finite _) = GT
  compare (Finite _) Infinite = LT
  compare (Finite a) (Finite b) = compare a b


type Grid s e = STArray s Position e    
type Potential s = Grid s Distance
type World s = Grid s Cell


data Event
  = Move Monster Position Position
  | Attack Monster Position Monster Position
  | Die Monster Position
  | Pass Monster Position
  | EndOfTurn Int Int Int String

instance Show Event where
  show (Move monster here destination) =
    show monster ++ " moves from " ++ show here ++ " to " ++ show destination ++ "."
  show (Attack monster here target position) =
    show monster ++" at " ++ show here
    ++  " attacks " ++ show target ++ " at " ++ show position ++ "."
  show (Die monster here) =
    show monster ++ " at " ++ show here ++ " dies."
  show (Pass monster here) =
    show monster ++ " at " ++ show here ++ " passes."
  show (EndOfTurn round elvesHP gobelinsHP grid) =
    "End of turn " ++ show round ++ ". "
    ++ "Elves have " ++ show elvesHP ++ " hitpoints. "
    ++ "Gobelins have " ++ show gobelinsHP ++ " hitpoints.\n"
    ++ grid
    ++ "\n"
  

data GameData s = GameData
    { round :: Int
    , maxPosition :: Position
    , world :: World s
    , gobelinPotential :: Potential s
    , elfPotential :: Potential s
    , elves :: Set Position
    , gobelins :: Set Position
    , events :: [ Event ]
    }


initialGameData :: Position -> ST s (GameData s)
initialGameData maxPosition  =
  do
    world <- Arr.newArray indices Empty
    gobelinPotential <- Arr.newArray indices Infinite
    elfPotential <- Arr.newArray indices Infinite
    return $
      GameData
      { round = 0
      , maxPosition = maxPosition
      , world = world
      , gobelinPotential = gobelinPotential
      , elfPotential = elfPotential
      , gobelins = Set.empty
      , elves = Set.empty
      , events = []
      }
  where
    indices = (Position (1,1), maxPosition)

    


data Effect s state r = Effect (state -> ST s (state, r))

instance Functor (Effect s state) where
  fmap f (Effect eff) = Effect $ \state -> do
    (newState, res) <- eff state
    return (newState, f res)

instance Applicative (Effect s state) where
  pure res = Effect $ \ state -> return (state, res)
  (Effect mF) <*> (Effect mArg) = Effect $ \state -> do
    (state1, f) <- mF state
    (state2, arg) <- mArg state1
    return (state2, f arg)
    
instance Monad (Effect s state) where
  return e = Effect $ \ state -> return (state, e)
  (Effect m) >>= fOfA = Effect $ \ state -> do
    (stateA, a) <- m state
    let Effect f = fOfA a
    f stateA
  
instance MonadState state (Effect s state) where
  get = Effect $ \ state -> return (state, state)
  put newState = Effect $ \ oldState -> return (newState, ())



liftST :: ST s r -> Effect s state r
liftST action = Effect $ \ state -> do
  res <- action
  return (state, res)

instance MArray (STArray s) e (Effect s state) where
  getBounds = liftST . ArrBase.getBounds 
  -- newArray bounds elt =
  --   liftST $ Arr.newArray bounds elt
  getNumElements = liftST . ArrBase.getNumElements
  unsafeRead array int = liftST (ArrBase.unsafeRead array int)
  unsafeWrite array int e = liftST (ArrBase.unsafeWrite array int e)



runEffect :: ST s state -> Effect s state r  -> ST s (state, r)
runEffect stAction (Effect effect) = do
  initState <- stAction
  effect initState
        


type GameEffect s r = Effect s (GameData s) r


getRound :: GameEffect s Int
getRound = round <$> get 

incrementRound :: GameEffect s ()
incrementRound = do
  state <- get
  put $ state { round = round state + 1 }

getWorld :: GameEffect s (World s)
getWorld = world <$> get

getMaxPosition :: GameEffect s Position
getMaxPosition = maxPosition <$> get

getPotentialFunction :: Race -> GameEffect s (Potential s)
getPotentialFunction Elf = elfPotential <$> get
getPotentialFunction Gobelin = gobelinPotential <$> get


getCell :: Position -> GameEffect s Cell
getCell here = do
  world <- getWorld
  Arr.readArray world here


updatePositionSet :: Race -> (Set Position -> Set Position) -> GameEffect s ()
updatePositionSet race f = do
  state <- get
  let set = access state
  put $ mutate state (f set)
  where
    (access,mutate) = case race of
      Elf -> (elves, \state set -> state { elves = set })
      Gobelin -> (gobelins, \state set -> state { gobelins = set })

addMonsterPosition :: Monster -> Position -> GameEffect s ()
addMonsterPosition monster position =
  updatePositionSet (race monster) (Set.insert position)

removeMonsterPosition :: Monster ->  Position -> GameEffect s ()
removeMonsterPosition monster position =
  updatePositionSet (race monster) (Set.delete position)
  

setCell :: Position -> Cell -> GameEffect s ()
setCell here content = do
  world <- getWorld
  Arr.writeArray world here content



getAdjacentPositions :: Position -> GameEffect s [ Position ]
getAdjacentPositions (Position (x,y)) = do
  Position (maxX,maxY) <- getMaxPosition
  let isValid (x,y) = 1 <= x && x <= maxX && 1 <= y && y <= maxY
  return . map Position . filter isValid $ [ (x,y-1), (x-1,y), (x+1,y), (x,y+1) ]


getPositions :: Race -> GameEffect s [ Position ]
getPositions Elf = Set.toList . elves <$> get
getPositions Gobelin = Set.toList . gobelins <$> get


getMonsterPositions :: GameEffect s [ Position ]
getMonsterPositions = do
  elfPositions <- elves <$> get
  gobelinPositions <- gobelins <$> get
  return $ Set.toList $ Set.union elfPositions gobelinPositions

getHitPoints :: Race -> GameEffect s Int
getHitPoints race = do
  positions <- getPositions race
  hitPoints <- forM positions getHitPoint
  return $ sum hitPoints
  where
    getHitPoint position = do
      Occupied monster <- getCell position
      return $ hitPoints monster


getAdjacentCells :: Position -> GameEffect s [ Cell ]
getAdjacentCells position = do
  positions <- getAdjacentPositions position
  forM positions getCell


getAdjacentEmptyPositions :: Position -> GameEffect s [ Position ]
getAdjacentEmptyPositions position = do
  neighbors <- getAdjacentPositions position
  filterM (isEmptyCell) neighbors
  where
    isEmptyCell pos = (== Empty) <$> getCell pos


getPotential :: Race -> Position -> GameEffect s Distance
getPotential race pos = do
  phi <- getPotentialFunction race
  Arr.readArray phi pos


setPotential :: Race -> Distance -> Position -> GameEffect s ()
setPotential race value position = do
  phi <- getPotentialFunction race
  Arr.writeArray phi position value

record :: Event -> GameEffect s ()
record newEvent = do
  state <- get
  put $ state { events = newEvent : events state }


recordMove :: Position -> Position -> GameEffect s ()
recordMove here destination = do
  Occupied monster <- getCell here
  record (Move monster here destination)

recordAttack :: Position -> Position -> GameEffect s ()
recordAttack here targetPosition = do
  Occupied monster <- getCell here
  Occupied target <- getCell targetPosition
  record (Attack monster here target targetPosition)

recordDie :: Position -> GameEffect s ()
recordDie here = do
  Occupied monster <- getCell here
  record (Die monster here)

recordPass :: Position -> GameEffect s ()  
recordPass here = do
  Occupied monster <- getCell here
  record (Pass monster here)

recordEndOfTurn :: GameEffect s ()
recordEndOfTurn = do
  round <- getRound
  elvesHP <- getHitPoints Elf
  gobelinsHP <- getHitPoints Gobelin
  grid <- showGrid (\ here -> cellToChar <$> getCell here)
  record (EndOfTurn round elvesHP gobelinsHP (intercalate "\n" grid))

-- Potential updates --

getVoronoiCell :: Race -> Position -> GameEffect s [ Position ]
getVoronoiCell race source = do
  d0 <- getPotential race source
  go [] [ source ] (incr d0)
  where
    isAtDistance dist position =
      (== dist) <$> getPotential race position
    go voronoiCell level dist = do     
      positions <- forM level getAdjacentEmptyPositions
      let adjacentPositions = removeDuplicate . concat $ positions
      nextLevel <- filterM (isAtDistance dist) adjacentPositions
      if null nextLevel then
        return voronoiCell
      else
        go (nextLevel ++ voronoiCell) nextLevel (incr dist)


propagateSeveral :: Race -> [ Position ] -> GameEffect s ()
propagateSeveral race positions = do
  emptyPositions <- concat <$> forM positions getAdjacentEmptyPositions
  updateSeveral race emptyPositions

propagate :: Race -> Position -> GameEffect s ()
propagate race here = do
  emptyPositions <- getAdjacentEmptyPositions here
  updateSeveral race emptyPositions


atomicUpdate :: Race -> Position -> GameEffect s [ Position ]
atomicUpdate race here = do
  neighbors <- getAdjacentPositions here
  minDist <- foldr min Infinite <$> forM neighbors (getPotential race)
  hereDist <- getPotential race here
  if incr minDist < hereDist then do
    setPotential race (incr minDist) here;
    getAdjacentEmptyPositions here
  else
    return []

updateSeveral :: Race -> [ Position ] -> GameEffect s ()
updateSeveral race positions = do
  nextPositions <- removeDuplicate . concat <$> forM positions (atomicUpdate race)
  if null nextPositions then
    return ()
  else
    updateSeveral race nextPositions


update :: Race -> Position -> GameEffect s ()
update race here = do
  updateSeveral race [ here ]
    
resetPotential :: Race -> [ Position ] -> GameEffect s ()
resetPotential race positions =
  forM_ positions (setPotential race Infinite)

-----------------------

stepDown :: Race -> Position -> GameEffect s [ Position ]
stepDown race here = do
  distance <- getPotential race here 
  neighbors <- getAdjacentEmptyPositions here
  filterM (isCloser distance) neighbors
  where
    isCloser distance position = do
      dist <- getPotential race position
      return $ dist == decr distance
      
shortestDescent :: Race -> [ Position ] -> GameEffect s (Maybe Position)
shortestDescent race [] = return Nothing
shortestDescent race positions@(here:_) = do
  distance <- getPotential race here
  if distance <= Finite 1 then
    return (Just here)
  else do
    nextLevel <- processNeighbors <$> forM positions (stepDown race)
    shortestDescent race nextLevel
  where
    processNeighbors = removeDuplicate . concat



-----------------------


concatGrid :: [ String ] -> [ String ] -> [ String ]
concatGrid = zipWith (\line1 line2 -> line1 ++ "  " ++ line2)

showGrid :: (Position -> GameEffect s Char) -> GameEffect s [ String ]
showGrid getter = do
  Position (maxX,maxY) <- getMaxPosition
  forM [1..maxY] $ \y ->
    forM [1..maxX] $ \ x ->
      getter (Position (x,y))

potentialToChar :: Distance -> Char
potentialToChar Infinite = ' '
potentialToChar (Finite d)
  | d < 10 = head (show d)
  | otherwise = '+'

getPotentialChar :: Race -> Position -> GameEffect s Char
getPotentialChar potentialRace position = do
  cell <- getCell position
  case cell of
    Wall -> return '#'
    Occupied monster ->
      if race monster == potentialRace then
        potentialToChar <$> getPotential potentialRace position
      else return 'X'
    Empty -> 
      potentialToChar <$> getPotential potentialRace position

      
      
showGame :: GameEffect s String
showGame = do
  worldGrid <- showGrid (\ here -> cellToChar <$> getCell here)
  elfGrid <- showGrid (getPotentialChar Elf)
  gobelinGrid <- showGrid (getPotentialChar Gobelin)
  return . intercalate "\n" $ worldGrid `concatGrid` elfGrid `concatGrid` gobelinGrid


-----------------------

getElves :: GameEffect s (Set Position)
getElves = elves <$> get

getGobelins :: GameEffect s (Set Position)
getGobelins = gobelins <$> get


placeMonster :: Monster -> Position -> GameEffect s ()
placeMonster monster here = do
  setCell here (Occupied monster);
  setPotential myRace (Finite 0) here;
  propagate myRace here;
  voronoiCell <- getVoronoiCell enemyRace here;
  resetPotential enemyRace (here:voronoiCell);
  updateSeveral enemyRace voronoiCell
  where
    myRace = race monster
    enemyRace = enemy $ myRace

unplaceMonster :: Monster -> Position -> GameEffect s ()
unplaceMonster monster here = do
  setCell here Empty;
  voronoiCell <- getVoronoiCell myRace here
  resetPotential myRace (here:voronoiCell);
  updateSeveral myRace (here:voronoiCell);
  update enemyRace here
  where
    myRace = race monster
    enemyRace = enemy myRace


displaceMonster :: Position -> Position -> GameEffect s ()
displaceMonster here destination = do
  Occupied monster <- getCell here
  let myRace = race monster
      enemyRace = enemy myRace
  setCell here Empty;
  setCell destination (Occupied monster);
  myVoronoiCell <- getVoronoiCell myRace here
  resetPotential myRace (here:myVoronoiCell);
  setPotential myRace (Finite 0) destination;
  propagate myRace destination;
  forM_ (here:myVoronoiCell) (update myRace);
  enemyVoronoiCell <- getVoronoiCell enemyRace destination;
  resetPotential enemyRace (destination:enemyVoronoiCell);
  update enemyRace here;
  updateSeveral enemyRace enemyVoronoiCell  

    

      

addMonster :: Monster -> Position -> GameEffect s ()
addMonster monster here = do
  addMonsterPosition monster here;
  placeMonster monster here
      
removeMonster :: Position -> GameEffect s ()
removeMonster here = do
  Occupied monster <- getCell here
  removeMonsterPosition monster here
  unplaceMonster monster here



move :: Position -> Position -> GameEffect s ()
move oldPosition newPosition = do
  recordMove oldPosition newPosition;
  Occupied monster <- getCell oldPosition
  removeMonsterPosition monster oldPosition;
  addMonsterPosition monster newPosition;
  displaceMonster oldPosition newPosition
  
die :: Position -> GameEffect s ()
die here = do
  recordDie here;
  removeMonster here


fight :: Position -> Position -> GameEffect s ()
fight hittingPosition hittedPosition = do
  recordAttack hittingPosition hittedPosition;
  Occupied hitting <- getCell hittingPosition
  Occupied hitted <- getCell hittedPosition
  let newMonster = hitted { hitPoints = hitPoints hitted - attackPower hitting }
  if hitPoints newMonster > 0 then do
    setCell hittedPosition (Occupied newMonster)
  else do
    die hittedPosition

pass :: Position -> GameEffect s ()
pass here = do
  recordPass here;
  return ()



getPositionedMonster :: Position -> GameEffect s (Maybe (Monster, Position))
getPositionedMonster here = do
  cell <- getCell here
  return $ case cell of
    Occupied m -> Just (m, here)
    Empty -> Nothing
    Wall -> Nothing


getAdjacentEnemyPositions :: Monster -> Position -> GameEffect s [ Position ]
getAdjacentEnemyPositions monster here = do
  positions <- getAdjacentPositions here
  process <$> forM positions getPositionedMonster
  where
    process =
        map snd
      . sortOn fst
      . filter ((/= race monster) . race . fst)
      . catMaybes


getPossibleMoves :: Monster -> Position -> GameEffect s [ Position ]
getPossibleMoves monster here = do
  emptyNeighbors <- getAdjacentEmptyPositions here
  distance <- foldr min Infinite <$> forM emptyNeighbors (getPotential enemyRace)
  if distance == Infinite then return [] else do
    directions <- filterM (isForward distance) emptyNeighbors
    destinations <- forM directions (shortestDescent enemyRace . pure)
    return $ best $ zip directions destinations
    where
      enemyRace = enemy $ race monster
      isForward distance position =
        (== distance) <$> getPotential enemyRace position
      best =
        map fst
        . sortOn snd
        . filter ((/= Nothing) . snd)


selectTarget :: Monster -> Position -> GameEffect s (Maybe Position)
selectTarget monster here = do
  enemiesInRange <- getAdjacentEnemyPositions monster here
  case enemiesInRange of
    lowestHitPointsEnemy:_ -> return $ Just lowestHitPointsEnemy
    [] -> return Nothing

tryAttack :: Monster -> Position -> GameEffect s () -> GameEffect s ()
tryAttack monster here elseAction = do
  target <- selectTarget monster here
  case target of
    Just enemy -> fight here enemy
    Nothing -> elseAction

tryMove :: Monster -> Position -> GameEffect s Position
tryMove monster here = do
  emptyCells <- getPossibleMoves monster here
  case emptyCells of
    bestCell:_ -> do
      move here bestCell;
      return bestCell
    [] ->
      return here
  

moveAndAttack :: Monster -> Position -> GameEffect s ()
moveAndAttack monster here = 
  tryAttack monster here $ do
  newPosition <- tryMove monster here
  tryAttack monster newPosition (pass newPosition)
  

activate :: Position -> GameEffect s ()
activate here = do
  cell <- getCell here
  case cell of
    Empty -> return ()
    Wall -> return ()
    Occupied monster -> moveAndAttack monster here 

endOfTurn :: GameEffect s ()
endOfTurn = do
  recordEndOfTurn;
  incrementRound
        

playRound :: GameEffect s ()
playRound = do
  monsterPositions <- getMonsterPositions
  forM monsterPositions activate
  endOfTurn



isOver :: GameEffect s Bool
isOver = do
  elves <- getElves
  gobelins <- getGobelins
  return (null elves || null gobelins)


stop :: GameEffect s ()
stop = return ()



play :: GameEffect s ()
play = do
  over <- isOver
  round <- getRound
  if over then stop
  else do
    playRound;
    play


start :: [ Position ] -> [ Position ] -> Int -> [ Position ] -> GameEffect s ()
start walls elves elfAttackPower gobelins = do
  Position (maxX,maxY) <- getMaxPosition
  forM walls (\pos -> setCell pos Wall)
  forM elves (createMonster $ Monster Elf 200 elfAttackPower)
  forM gobelins (createMonster $ Monster Gobelin 200 3)
  propagateSeveral Elf elves
  propagateSeveral Gobelin gobelins
  where
    createMonster monster position = do
      addMonsterPosition monster position
      setCell position (Occupied monster)
      setPotential (race monster) (Finite 0) position




makeGameData :: [ String ] -> ST s (GameData s)
makeGameData rows =
  initialGameData (Position (maxX,maxY))
  where
    maxY = length rows
    maxX = length $ head rows

positionsOfChar :: [ String ] -> Char -> [ Position ]
positionsOfChar rows char =
  concat
  . zipWith mkPairs [1..]
  . map extractIndices
  $ rows
  where
    extractIndices row =
      map fst
      . filter (\(i,cell) -> cell == char)
      . zip [1..]
      $ row
    mkPairs y xs = map (\x -> Position (x,y)) xs



gameValue :: GameEffect s Int
gameValue = do
  round <- getRound
  elvesHP <- getHitPoints Elf
  gobelinsHP <- getHitPoints Gobelin
  return $ (round-1) * (elvesHP + gobelinsHP)

game :: Int -> String -> ST s (GameData s, String)
game elfAttackPower text =
  runEffect (makeGameData rows) $ do
    start wallPositions elfPositions elfAttackPower gobelinPositions;
    play;
    allEvents <- events <$> get
    return $ unlines $ map show $ reverse allEvents
    -- show <$> gameValue
    -- show <$> getAdjacentEmptyPositions (15,5)
    -- minDist <- foldr min Infinite <$> forM emptyPositions (getPotential race)
    -- hereDist <- getPotential race here
  where
    rows = lines text
    wallPositions = positionsOfChar rows '#'
    elfPositions = positionsOfChar rows 'E'
    gobelinPositions = positionsOfChar rows 'G'





processPart1 :: String -> String
processPart1 input =
  runST (snd <$> game 3 input)
      

processPart2 :: String -> String
processPart2 input =
  runST (snd <$> game optimalPower input)
  where
    optimalPower = go 3 200
    go n p
      | n == p = n
      | otherwise =
        let middle = (n + p) `div` 2 in
        let allEvents = runST (events . fst <$> game middle input) in
        if noElfDies allEvents then go n middle
        else go (middle+1) p
    noElfDies events = all (not . isElfDeath) events
    isElfDeath (Die (Monster Elf _ _) _) = True
    isElfDeath _ = False
