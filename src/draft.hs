
getCells :: State s -> ST s [ (Position, Cell) ]
getCells state =
  sortCells <$> Arr.getAssocs (world state)
  where
    sortCells = sortBy compareCells
    compareCells (pos1,_) (pos2,_) = comparePosition pos1 pos2

getMonsters :: State s -> ST s [ Monster ]
getMonsters state = 
  fmap (catMaybes . map (cellToMonster . snd)) $ getCells state

getElves :: State s -> ST s [ Monster ]
getElves state = 
  fmap (filter (\m -> race m == Elf)) $ getMonsters state

getGobelins :: State s -> ST s [ Monster ]
getGobelins state =
  fmap (filter (\m -> race m == Gobelin)) $ getMonsters state


  
gridToString :: (e -> Char) -> [ (Position, e) ] -> String
gridToString showElt =
  unlines
--  . map (concat . map (\(pos,e) -> showElt e : show pos ++ " "))
  . map (map (showElt . snd))
  . groupBy (\(pos1,_) (pos2,_) -> snd pos1 == snd pos2)
  . sortBy (\ (pos1,_) (pos2,_) -> comparePosition pos1 pos2)


showArray :: (e -> Char) -> STArray s Position e -> ST s String
showArray showElt array = do
  elems <- Arr.getAssocs array
  return $ gridToString showElt elems


concatGrids :: String -> String -> String
concatGrids grid1 grid2 =
  unlines
  $ zipWith (\l1 l2 -> l1 ++ "  " ++ l2) (lines grid1) (lines grid2)
  

stateToString :: State s -> ST s String
stateToString state = do 
  cellGrid <- showArray cellToChar $ world state
  elfDistGrid <- showArray distToChar $ elfDist state
  gobDistGrid <- showArray distToChar $ gobelinDist state
  return
    $ "round " ++ show (round state) ++ "\n"
    ++ cellGrid `concatGrids` elfDistGrid `concatGrids` gobDistGrid



adjacentPositions :: Position -> [ Position ]
adjacentPositions (x,y) =
  filter isInBound [ (x,y-1), (x-1,y), (x+1,y), (x,y+1)]
  where
    isInBound (x,y) = 1 <= x && x <= dim && 1 <= y && y <= dim

adjacentEmptyPositions :: Position -> State s -> ST s [ Position ]
adjacentEmptyPositions position state = 
  filterM (\pos -> fmap isEmpty $ getCell pos state)
  . adjacentPositions
  $ position



updateDistance ::
  STArray s Position Distance
  -> State s
  -> Position
  -> ST s [ Position ]
updateDistance distances state here = do
  let neighbors = adjacentPositions here
  emptyNeighbors <- adjacentEmptyPositions here state
  mini <- fmap incr $ minDist neighbors
  current <- Arr.readArray distances here
  if mini < current then do
    Arr.writeArray distances here mini;
    return emptyNeighbors
  else
    return []
  where
    minDist neighbors =
      fmap (foldr min Infinite)
      $ forM neighbors (Arr.readArray distances) 
      

computeDistances :: [ Position ] -> STArray s Position Distance -> State s -> ST s ()
computeDistances [] distances state = return ()
computeDistances positions distances state = do
  nextPositions <- forM positions (updateDistance distances state)
  computeDistances (concat nextPositions) distances state



getAdjacentIncrCells :: STArray s Position Distance -> Position -> ST s [ Position ]
getAdjacentIncrCells distances here = do
  incrDist <- incr <$> Arr.readArray distances here
  filterM (isAtDist incrDist) neighbors
  where 
    neighbors = adjacentPositions here
    isAtDist incrDist cell = do
      d <- Arr.readArray distances cell
      return (d == incrDist)

getVoronoiCell :: STArray s Position Distance -> Position -> ST s [ Position ]
getVoronoiCell distances here =
  go [] [ here ]
  where
    go inVoronoi level = do
      nextLevel <- concat <$> forM level (getAdjacentIncrCells distances)
      if null nextLevel then
        return inVoronoi
      else
        go (nextLevel ++ inVoronoi) nextLevel


resetVoronoiCell :: STArray s Position Distance -> Position -> ST s [Position]
resetVoronoiCell distances here = do
  voronoiCell <- getVoronoiCell distances here
  forM_ (here:voronoiCell) (\pos -> Arr.writeArray distances pos Infinite);
  return voronoiCell



getDistances ::
  Monster
  -> State s
  -> (STArray s Position Distance, STArray s Position Distance)
getDistances monster state = 
  case race monster of
    Elf -> (elfDist state, gobelinDist state)
    Gobelin -> (gobelinDist state, elfDist state)

remove :: Monster -> State s -> ST s ()
remove monster state = do
  Arr.writeArray (world state) pos Empty;
  itsVoronoiCell <- resetVoronoiCell distances pos
  computeDistances (pos:itsVoronoiCell) distances state;
  computeDistances [pos] enemyDistances state
  where
    pos = position monster
    (distances, enemyDistances) = getDistances monster state

put :: Monster -> State s -> ST s ()
put monster state = do
  Arr.writeArray (world state) pos (Occupied monster);
  enemyVoronoiCell <- resetVoronoiCell enemyDistances pos
  Arr.writeArray distances pos (Finite 0);
  adjacentPositions <- adjacentEmptyPositions pos state
  computeDistances adjacentPositions distances state;
  computeDistances enemyVoronoiCell enemyDistances state
   where
    pos = position monster
    (distances, enemyDistances) = getDistances monster state

 

move :: Monster -> Position -> State s -> ST s ()
move monster toPos state = do
  remove monster state;
  put (monster { position = toPos }) state
  where
    fromPos = position monster
    (distances, enemyDistances) =
      case race monster of
        Elf -> (elfDist state, gobelinDist state)
        Gobelin -> (gobelinDist state, elfDist state)



initMonsterDistance :: STArray s Position Distance -> [Position] -> State s -> ST s ()
initMonsterDistance distances monsterPositions state = do
  positions <- fmap concat $ forM monsterPositions getAdjacents
  computeDistances positions distances state
  where
    getAdjacents pos = adjacentEmptyPositions pos state


initElfDistance :: State s -> ST s ()
initElfDistance state = do
  elves <- getElves state
  initMonsterDistance (elfDist state) (map position elves) state

initGobelinDistance :: State s -> ST s ()
initGobelinDistance state = do
  gobelins <- getGobelins state
  initMonsterDistance (gobelinDist state) (map position gobelins) state


toArray :: (MArray a Cell m) => String -> m (a Position Cell)
toArray input = do
  array <- Arr.newArray ((1,1),(dim,dim)) Wall
  forM_ cells (\(cell,pos) -> Arr.writeArray array pos cell);
  return array
  where
    cells =
      map (\(pos,symbol) -> (createCell symbol pos, pos))
      . zip positions
      . filter ((/=) '\n')
      $ input
      

makeState :: String -> ST s (State s)
makeState input = do
  world <- toArray input
  gobDist <- Arr.mapArray (dist0 isGobelin) world
  elfDist <- Arr.mapArray (dist0 isElf) world
  let state = State
        { round = 0
        , world = world
        , gobelinDist = gobDist
        , elfDist = elfDist
        }
  initElfDistance state;
  initGobelinDistance state;
  return state
  where
    dist0 condition cell = if condition cell then Finite 0 else Infinite



fight :: State s -> Monster -> Monster -> ST s ()
fight state hitting hitted =
  Arr.writeArray (world state) (position hitted) newCell
  where
    newHitPoints = hitPoints hitted - attackPower hitting
    newCell =
      if newHitPoints <= 0 then
        Empty
      else
        Occupied $ hitted { hitPoints = newHitPoints }



findEnemy :: Race -> [ Cell ] -> Maybe Monster
findEnemy race cells =
   listToMaybe
   . sortOn hitPoints
   . map (\ (Occupied m) -> m)
   . filter (isEnemyOf race)
   $ cells


findMoveTowardEnemy :: State s -> Race -> [ Position ] -> ST s (Maybe Position)
findMoveTowardEnemy state race positions = do
  emptyPosition <- filterM hasEmptyCell positions
  distCells <- forM emptyPosition attachDistance
  return
    . fmap fst
    . listToMaybe
    . sortOn closestToEnemy
    . filter (isFinite . snd)
    $ distCells 
  where
    hasEmptyCell position = do
      cell <- Arr.readArray (world state) position
      return (cell == Empty)
    attachDistance position = do
      dist <- Arr.readArray distances position
      return (position, dist)
    closestToEnemy = snd   
    distances
      | race == Elf = gobelinDist state
      | race == Gobelin = elfDist state
        
    
activateMonster :: State s -> Monster -> ST s ()
activateMonster state monster = do
  adjacentCells <- forM neighborPos (Arr.readArray (world state))
  case findEnemy (race monster) adjacentCells of
    Just enemy -> fight state monster enemy
    Nothing -> do
      possibleMove <- findMoveTowardEnemy state (race monster) neighborPos
      case possibleMove of
        Just toPos -> move monster toPos state
        Nothing -> return ()
  where
    neighborPos = adjacentPositions (position monster)


activateCellAtPosition :: State s -> Position -> ST s ()
activateCellAtPosition state here = do
  cell <- Arr.readArray (world state) here
  case cell of
    Occupied m -> activateMonster state m
    Wall -> return ()
    Empty -> return ()
    

makeRound :: State s -> ST s (State s)
makeRound state = do
  monsterPositions <- map position <$> getMonsters state
  forM_ monsterPositions (activateCellAtPosition state);
  return $ state { round = round state + 1 }


data Stats
  = Stats
    { elvesHitPoints :: Int
    , gobelinsHitPoints :: Int
    , worldCopy :: Array Position Cell
    , asString :: String
    }


toStats :: State s -> ST s Stats
toStats state = do
  elves <- filter ((== Elf) . race) <$> getMonsters state
  gobelins <- filter ((== Gobelin) . race)  <$> getMonsters state
  worldCopy <- Arr.freeze (world state)
  string <- stateToString state
  return $ Stats (sumHitPoints elves) (sumHitPoints gobelins) worldCopy string
    where
      sumHitPoints monsters =
        sum $ map hitPoints monsters

        
instance Show Stats where
  show stats =
    asString stats ++ "\n"
    ++ "Elves hitpoints: " ++ show (elvesHitPoints stats) ++ "\n"
    ++ "Gobelins hitpoints: " ++ show (gobelinsHitPoints stats) ++ "\n"


type Game s = (State s, [ Stats ])


isOver :: Game s -> ST s Bool
isOver (state, []) = return False
isOver (state, stats:_) =
  return $ elvesHitPoints stats == 0 || gobelinsHitPoints stats == 0

isPastRound :: Int -> Game s -> ST s Bool
isPastRound n (state, _) =
  return $ round state >= n



repeatUntil :: (Game s -> ST s Bool) -> (Game s -> ST s (Game s)) -> Game s -> ST s (Game s)
repeatUntil isOver evolve initGame = do
  isEnding <- isOver initGame
  if isEnding then
    return initGame
  else do
    nextGame <- evolve initGame
    repeatUntil isOver evolve nextGame

gameStep :: Game s -> ST s (Game s)
gameStep (state,stats) = do
  nextState <- makeRound state
  stat <- toStats nextState
  return (nextState, stat:stats)
  
  

-- processPart1 :: String -> String
-- processPart1 input =
--   runST process
--   where
--     process = do
--       state0 <- makeState input
--       str0 <- stateToString state0
--       state1 <- makeRound state0
--       str1 <- stateToString state1
--       state2 <- makeRound state1
--       str2 <- stateToString state2
--       return $ str0 ++ str1 -- ++ str2
      
    
   
processPart1 :: String -> String
processPart1 input =
  show $ runST
  (do
      state0 <- makeState input
      stats <- toStats state0
      let game0 = (state0, [stats])
--       game1 <- gameStep game0
--       game2 <- gameStep game1
--       game3 <- gameStep game2
--       game4 <- gameStep game3
      gameN <- repeatUntil (isPastRound 100) gameStep game0
      return $ snd gameN
  )
