module Day20 (processPart1, processPart2) where

import Text.Trifecta (parseString, Result(Success,Failure), ErrInfo, Parser)
import Text.Parser.Token (whiteSpace, integer, symbol, symbolic, textSymbol)
import Text.Parser.Combinators (some, many, eof, try, sepBy)
import Text.Parser.Char (oneOf)
import Control.Applicative ((<|>), (<*>), (*>), (<*))

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


data Direction
  = N
  | S
  | E
  | W
  deriving Show
    
data Regex
  = Dir Direction
  | Concat [ Regex ]
  | Branch [ Regex ]
  deriving Show
             
inputParser :: Parser Regex
inputParser =
  symbol "^" *> concat <* symbol "$"
  where
    concat = Concat <$> many factor
    factor = parenthesized <|> single
    parenthesized = (try $ symbol "(") *> branches <* (symbol ")")
    branches = Branch <$> (concat `sepBy` (symbol "|"))
    single = toDir <$> oneOf "NSEW"
    toDir 'N' = Dir N
    toDir 'S' = Dir S
    toDir 'E' = Dir E
    toDir 'W' = Dir W


type Position = (Int, Int)

addPosition :: Position -> Position -> Position
addPosition (x1,y1) (x2,y2) = (x1+x2, y1+y2)

move :: Direction -> Position -> Position
move N (x,y) = (x,y+1)
move S (x,y) = (x,y-1)
move E (x,y) = (x+1,y)
move W (x,y) = (x-1,y)

type Graph = Map Position [ Position ]

emptyGraph :: Graph
emptyGraph = Map.empty

neighbors :: Graph -> Position -> [ Position ]
neighbors graph vertex =
  Map.findWithDefault [] vertex graph

  
addArc :: Graph -> Position -> Position -> Graph
addArc graph fromPos toPos =
  if toPos `elem` oldNeighbors then graph
  else Map.insert fromPos (toPos:oldNeighbors) graph
  where
    oldNeighbors = Map.findWithDefault [] fromPos graph
    

addEdge :: Graph -> Position -> Position -> Graph
addEdge graph pos1 pos2 =
  addArc (addArc graph pos2 pos1) pos1 pos2


edge :: Position ->  Position -> (Position, Position)
edge pos1 pos2
  | pos1 < pos2 = (pos1, pos2)
  | otherwise = (pos2, pos1)

    

type Path = [ Direction ]

allPaths :: [ Path ] -> Regex -> [ Path ]
allPaths paths (Dir direction) =
  map (direction:) paths
allPaths paths (Concat subs) =
  foldr (flip allPaths) paths subs
allPaths paths (Branch subs) =
  concat $ map (allPaths paths) subs


addPathToGraph :: Path -> Graph -> Graph
addPathToGraph path graph =
  fst . foldr step (graph, (0,0)) $ reverse path
  where
    step direction (upGraph, here) =
      let next = move direction here in
      (addEdge upGraph here next, next)


makeGraph :: Regex -> Graph
makeGraph =
  foldr addPathToGraph emptyGraph
  . allPaths [[]]




data Walk =
  Walk
  { relativePositions :: Set Position,
    relativeEdges :: Set (Position, Position)
  }

stay :: Walk
stay = Walk (Set.singleton (0,0)) Set.empty

walkDirection :: Direction -> Walk
walkDirection dir =
  Walk (Set.singleton newPos) (Set.singleton $ edge oldPos newPos)
  where
    oldPos = (0,0)
    newPos = move dir oldPos

follow :: Walk -> Walk -> Walk
follow walk1 walk2 =
  Walk newPositions allEdges
  where
    newPositions =
      Set.fromList [ v1 `addPosition` v2
                   | v1 <- Set.toList $ relativePositions walk1
                   , v2 <- Set.toList $ relativePositions walk2
                   ]
    allEdges =
      foldr Set.insert (relativeEdges walk1) $
      [ edge (v `addPosition` a) (v `addPosition` b)
      | v <- Set.toList $ relativePositions walk1
      , (a,b) <- Set.toList $ relativeEdges walk2
      ]
      
choose :: [ Walk ] -> Walk
choose walks =
  Walk allPositions allEdges
  where
    allPositions = Set.unions . map relativePositions $ walks
    allEdges = Set.unions . map relativeEdges $ walks  


reduce :: Regex -> Walk
reduce (Dir direction) = walkDirection direction
reduce (Concat subs) = foldr follow stay . map reduce $ subs
reduce (Branch subs) = choose . map reduce $ subs

graphFromWalk :: Walk -> Graph
graphFromWalk =
  Set.foldr insertEdge emptyGraph . relativeEdges 
  where
    insertEdge (pos1, pos2) graph = addEdge graph pos1 pos2

-- walk :: (Set Position, Graph) -> Regex -> (Set Position, Graph)

-- walk (heres, graph) (Dir direction) =
--   ( Set.fold insert Set.empty heres
--   , Set.fold openDoor graph heres
--   )
--   where
--     insert here = Set.insert (move direction here)
--     openDoor here graph = addEdge graph here (move direction here)

-- walk status (Concat subs) =
--   foldr (flip walk) status subs

-- walk (heres, graph) (Branch branches) =  
--   unitePositions $ foldr tryBranch ([], graph) branches
--   where
--     tryBranch branch (dests, graph) = 
--       let (dest, graph) = walk (heres, graph) branch in
--       (dest:dests, graph)
--     unitePositions (sets, graph) = 
--       (Set.unions sets, graph)


type Distance = Int

bfs :: Position -> Graph -> Map Position Distance
bfs source graph =
  go (Set.singleton source) (Map.singleton source 0) 0
  where
    go currentVertices distances d
      | Set.null currentVertices = distances
      | otherwise =
        let newDistances = Set.fold (setDistance d) distances currentVertices in
        let nextLevel = explore newDistances currentVertices in
        go nextLevel newDistances (d+1)
    setDistance d vertex distances =
      Map.insert vertex d distances
    explore distances level =
      Set.fold (exploreVertex distances) Set.empty level
    exploreVertex distances vertex nextLevel =
      foldr Set.insert nextLevel
      . filter (\v -> not (Map.member v distances))
      . neighbors graph
      $ vertex
    

-- distances :: Regex -> Map Position Distance
-- distances regex =
--   bfs (0,0) graph
--   where
--     (_,graph) = walk (Set.singleton (0,0), emptyGraph) regex

buildDistances :: Regex -> Map Position Distance
buildDistances = bfs (0,0) . graphFromWalk . reduce


processPart1 :: String -> String
processPart1 input =
  case parseString inputParser mempty input of
    Success regex ->
      show . maximum . Map.elems . buildDistances  $ regex
    Failure error -> show error


processPart2 :: String -> String
processPart2 input =
  case parseString inputParser mempty input of
    Success regex ->
      show . length . filter (>= 1000) . Map.elems . buildDistances  $ regex
    Failure error -> show error
