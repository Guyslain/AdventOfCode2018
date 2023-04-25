module Day25 (processPart1, processPart2) where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set


type Point = (Int, Int, Int, Int)

distance :: Point -> Point -> Int
distance (a,b,c,d) (e,f,g,h) =
  abs (a-e) + abs (b-f) + abs (c-g) + abs (d-h)

data Graph =
  Graph
  { vertices :: Set Point
  , neighbors :: Map Point (Set Point)
  }

emptyGraph :: Graph
emptyGraph = Graph Set.empty Map.empty

addEdge :: Point -> Point -> Graph -> Graph
addEdge u v graph =
  graph {
    neighbors =
      Map.adjust (Set.insert v) u $
      Map.adjust (Set.insert u) v $
      neighbors graph
    }

addVertex :: Point -> Graph -> Graph
addVertex point graph =
  Graph
      (Set.insert point (vertices graph))
      (Map.insert point Set.empty (neighbors graph))
  
    
add :: Point -> Graph ->  Graph
add point graph
  | Set.member point (vertices graph) = graph
  | otherwise =
    foldr (addEdge point) (addVertex point graph) closePoints
    where
      newVertices = Set.insert point (vertices graph)
      closePoints =
        Set.filter (\u -> distance u point <= 3) (vertices graph)
  

search :: Graph -> Set Point -> Set Point -> [Point] -> (Set Point, Set Point)
search graph reached component [] = (reached, component)
search graph reached component (point:points)
  | Set.member point reached = search graph reached component points
  | otherwise =
    search graph (Set.insert point reached) (Set.insert point component) newPoints
    where
      newPoints = foldr (:) points neighborhood
      neighborhood = Map.findWithDefault Set.empty  point $ neighbors graph

components :: Graph -> [ Set Point ]
components graph =
  go (Set.toList $ vertices graph) Set.empty []
  where 
    go [] reached components = components
    go (source:points) reached components 
      | Set.member source reached = go points reached components
      | otherwise =
          go points newlyReached (newComponent:components)
          where
            (newlyReached, newComponent) = search graph reached Set.empty [source]


showComponent :: Set Point -> String
showComponent =
  unwords . map show . Set.toList



readPoint :: String -> Point
readPoint =
  toPoint . map read . words . map commaToSpace 
  where
    commaToSpace ',' = ' '
    commaToSpace c = c
    toPoint [a,b,c,d] = (a,b,c,d)
    
readInput :: String -> Graph
readInput =
  foldr add emptyGraph . map readPoint . lines

processPart1 :: String -> String
processPart1 input = unlines . map showComponent . components $ readInput input

processPart2 :: String -> String
processPart2 input = input
