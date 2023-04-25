module Day13 (processPart1, processPart2) where

import Prelude hiding (Either(Left,Right))
import Data.Array
import Data.List (sort, find)



data Track =
    Vertical
  | Horizontal
  | Crossing
  | Curve
  | BackCurve
  | Empty deriving Eq

type Tracks = Array (Int,Int) Track

instance Show Track where
  show Vertical = "|"
  show Horizontal = "-"
  show Crossing = "+"
  show Curve = "/"
  show BackCurve = "\\"
  show Empty = " "

toTrack :: Char -> Track
toTrack '|' = Vertical
toTrack 'v' = Vertical
toTrack '^' = Vertical
toTrack '-' = Horizontal
toTrack '<' = Horizontal
toTrack '>' = Horizontal
toTrack '+' = Crossing
toTrack '/' = Curve
toTrack '\\' = BackCurve
toTrack _ = Empty




data Direction = Up | Left | Down | Right deriving (Show, Eq, Ord, Enum, Bounded)
data Turn = TurnLeft | GoStraight | TurnRight deriving (Ord, Eq, Enum, Bounded)
instance Show Turn where
  show TurnLeft = "Left"
  show TurnRight = "Right"
  show GoStraight = "Straight"



data Cart = Cart
  { position :: (Int, Int)
  , direction :: Direction
  , nextTurn :: Turn
  } deriving (Show, Eq)
 

instance Ord Cart where
  cart1 <= cart2 =
    position cart1 < position cart2    
    || (position cart1 == position cart2
        && (direction cart1 < direction cart2
            || (direction cart1 == direction cart2 && nextTurn cart1 <= nextTurn cart2)
            )
        )

cartSymbol :: Cart -> Char
cartSymbol cart =
  case direction cart of
    Left -> '<'
    Up -> '^'
    Right -> '>'
    Down -> 'v'
    

toCart :: ((Int,Int),Char) -> Cart
toCart ((i,j), c) =
  Cart
    (i,j)
    (case c of
        '<' -> Left
        '>' -> Right
        '^' -> Up
        'v' -> Down
    )
    TurnLeft


makeTurn :: Direction -> Turn -> Direction
dir `makeTurn` TurnLeft = cyclicSucc dir
dir `makeTurn` TurnRight = cyclicPred dir
dir `makeTurn` GoStraight = dir

curve :: Direction -> Direction
curve Up = Right
curve Left = Down
curve Down = Left
curve Right = Up

backCurve :: Direction -> Direction
backCurve Up = Left
backCurve Left = Up
backCurve Down = Right
backCurve Right = Down

  
cyclicSucc :: (Eq a, Enum a, Bounded a) => a -> a
cyclicSucc e
  | e == maxBound = minBound
  | otherwise = succ e
cyclicPred :: (Eq a, Enum a, Bounded a) => a -> a
cyclicPred e
  | e == minBound = maxBound
  | otherwise = pred e 


moveCart :: Tracks -> Cart -> Cart
moveCart tracks (Cart (i,j) direction turn) =
  Cart newPosition newDirection newTurn
  where
    newPosition = case direction of
       Up -> (i-1,j)
       Down -> (i+1,j)
       Left -> (i,j-1)
       Right -> (i,j+1)
    track = tracks!newPosition
    newDirection = case track of
      Crossing -> direction `makeTurn` turn
      BackCurve-> backCurve direction 
      Curve ->  curve direction
      _ -> direction
    newTurn = if track == Crossing then cyclicSucc turn else turn


data Result =
  Ok [ Cart ]
  | Collision (Int,Int)

moveSortedCarts :: Tracks -> [ Cart ] -> [ Cart ] -> Result
moveSortedCarts tracks moved [] = Ok moved
moveSortedCarts tracks moved (cart:carts) =
  if collision then Collision (position newCart)
  else moveSortedCarts tracks (newCart:moved) carts
  where
    newCart = moveCart tracks cart
    collision =
      any (\c -> position c == position newCart) (moved ++ carts)      

tick :: Tracks -> [ Cart ] -> Result
tick tracks =
  moveSortedCarts tracks [] 
  . sort
      

findFirstCollision :: Tracks -> [ Cart ] -> (Int, Int)
findFirstCollision tracks carts =
  case tick tracks carts of
    Collision position -> position
    Ok carts -> findFirstCollision tracks carts

  

      
indexMatrix :: [[a]] -> [((Int,Int),a)]
indexMatrix =
  concat
  . map (\(i,row) -> zip (zip (repeat i) [1..]) row)
  . zip [1..]



showSituation :: Array (Int,Int) Track -> [ Cart ] -> String
showSituation tracks carts =
  unlines $ 
  [ [ symbol (i, j) | j <- [minJ..maxJ] ]
  | i <- [minI..maxI]
  ]
  where
    ((minI,minJ),(maxI,maxJ)) = bounds tracks
    symbol pos =
      case find ((==) pos . position) carts of
        Just cart -> cartSymbol cart
        Nothing -> head . show $ tracks!pos

          

showIteration :: Int -> Tracks -> [ Cart ] -> String
showIteration 0 tracks carts =
  showSituation tracks carts
showIteration n tracks carts =
  case tick tracks carts of
    Collision position -> showPosition position
    Ok carts -> showIteration (n-1) tracks carts

showPosition :: (Int,Int) -> String
showPosition (i,j) = show (j-1) ++ "," ++ show (i-1)


readInstance :: String -> (Tracks, [ Cart ])
readInstance input = (tracks, carts)
  where
    rows = lines input
    rowCount = length rows
    columnCount = maximum . map length $ rows
    indexed = indexMatrix rows
    tracks = fmap toTrack $ array ((1,1), (rowCount,columnCount)) indexed
    carts =
      map toCart
      . filter (\(_,c) -> c `elem` "<>v^")
      $ indexed

processPart1 :: String -> String
processPart1 input =
  showPosition $ findFirstCollision tracks carts
  where
    (tracks, carts) = readInstance input
    
moveSortedCarts2 :: Tracks -> [ Cart ] -> [ Cart ] -> [ Cart ]
moveSortedCarts2 tracks moved [] = moved
moveSortedCarts2 tracks moved (cart:carts) =
  if collision then
    moveSortedCarts2 tracks (filterColliding moved) (filterColliding carts)
  else
    moveSortedCarts2 tracks (newCart:moved) carts
  where
    newCart = moveCart tracks cart
    colliding =  (== position newCart) . position
    collision = any colliding  (moved ++ carts)      
    filterColliding = filter (not . colliding)


tick2 :: Tracks -> [ Cart ] -> [ Cart ]
tick2 tracks = moveSortedCarts2 tracks [] . sort


findLastCart :: Tracks -> [ Cart ] -> Cart
findLastCart tracks [single] = single
findLastCart tracks carts = findLastCart tracks . tick2 tracks $ carts

processPart2 :: String -> String
processPart2 input =
  showPosition . position $ findLastCart tracks carts
  where
    (tracks, carts) = readInstance input

showFirstIterations :: String -> String
showFirstIterations input =
  concat . map (showSituation tracks) . take 5 . iterate (tick2 tracks) $ carts
  where
    (tracks, carts) = readInstance input
