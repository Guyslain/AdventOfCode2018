module Day9 (processPart1) where

import Prelude hiding (repeat)
import qualified Data.List as List
import qualified Data.Map as Map 

players :: Int
players = 466
marbles :: Int
marbles = 7143600

incrementPlayer :: Player -> Player
incrementPlayer p
  | p == players = 1
  | otherwise = p + 1


data Deque a = Deque [ a ] [ a ]

empty :: Deque a
empty = Deque [] [] 
deque :: [ a ] -> [ a ] -> Deque a
deque [] suffix = Deque (List.reverse suffix) []
deque prefix suffix = Deque prefix suffix

cons :: a -> Deque a -> Deque a
cons a (Deque pref suff) = Deque (a:pref) suff 
snoc :: Deque a -> a -> Deque a
snoc (Deque pref suff) a = deque pref (a:suff)
null :: Deque a -> Bool
null (Deque pref suff) = List.null pref && List.null suff

view :: Deque a -> Maybe (a, Deque a)
view (Deque [] []) = Nothing
view (Deque (h:pref) suff) = Just (h, deque pref suff)
weiv :: Deque a -> Maybe (Deque a, a)
weiv (Deque pref (h:suff)) = Just (Deque pref suff, h)
weiv (Deque [] []) = Nothing
weiv (Deque [single] []) = Just (empty, single)
weiv (Deque all []) = Just (deque pref suff, last)
  where
    (pref,ffus) = List.splitAt (length all `div` 2) all
    last:suff = List.reverse ffus

   
    
type Player = Int 
type Position = Deque Int
type Score = Map.Map Player Int
data Game = Game
  { position :: Position
  , score :: Score
  , nextPlayer :: Player
  , nextMarble :: Int
  }

startingPosition :: Game
startingPosition =
  Game
  { position = cons 0 empty,
    score = Map.empty,
    nextPlayer = 1,
    nextMarble = 1
    }

repeat :: Int -> (a -> a) -> a -> a
repeat 0 f = id
repeat n f = repeat (n-1) f . f 

rotate :: Int -> Position -> Position
rotate 1 deque =
  case view deque of
    Nothing -> empty
    Just (head, tail) -> snoc tail head
rotate k xs = repeat k (rotate 1) xs

rotateBack :: Int -> Position -> Position
rotateBack 1 deque =
  case weiv deque of
    Nothing -> empty
    Just (tail, head) -> cons head tail
rotateBack k deque = repeat k (rotateBack 1) deque 


updateScore :: Player -> Int -> Score -> Score
updateScore player points =
  Map.alter addPoints player
  where
    addPoints Nothing = Just points
    addPoints (Just previousPoints) = Just (points + previousPoints)

normalTurn :: Game -> Game
normalTurn (Game position score nextPlayer nextMarble) =
  Game {
    position = (nextMarble `cons`) . rotate 2 $ position,
    score = score,
    nextPlayer = incrementPlayer nextPlayer,
    nextMarble = nextMarble + 1
  }

specialTurn :: Game -> Game
specialTurn (Game position score nextPlayer nextMarble) =
  Game {
    position = finalPosition,
    score = updateScore nextPlayer (nextMarble + removedMarble) score,
    nextPlayer = incrementPlayer nextPlayer,
    nextMarble = nextMarble + 1
  }
  where
    Just (removedMarble, finalPosition) = view (rotateBack 7 position)

play :: Game -> Game
play game
  | nextMarble game > marbles = game
  | nextMarble game `rem` 23 == 0 = play . specialTurn $ game
  | otherwise = play . normalTurn $ game 


bestScore :: Game -> Int
bestScore (Game _ score _ _) = maximum $ Map.elems score

processPart1 :: String -> String
processPart1 _ = show . bestScore . play $ startingPosition
