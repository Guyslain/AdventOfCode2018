module Day18 (processPart1, processPart2) where



import Data.Array.IArray (Array, IArray,(!))
import qualified Data.Array.IArray as Arr
import Data.Map (Map)
import qualified Data.Map as M

data Cell
  = Open
  | Tree
  | Lumberyard
  deriving (Eq, Ord)


charFromCell :: Cell -> Char
charFromCell Open = '.'
charFromCell Tree = '|'
charFromCell Lumberyard = '#'

cellFromChar :: Char -> Cell
cellFromChar '.' = Open
cellFromChar '|' = Tree
cellFromChar '#' = Lumberyard

type Position = (Int, Int)

size :: Int
size = 50

allPositions :: [ (Int, Int) ]
allPositions = [ (x,y) | y <- [1..size], x <- [1..size] ]

minPosition :: Position
minPosition = (1,1)

maxPosition :: Position
maxPosition = (size,size)


readArray :: String -> Array Position Cell
readArray =
  Arr.array (minPosition, maxPosition)
  . concat
  . zipWith addY [1..size]
  . map (zip [1..size])
  . map (map cellFromChar)
  . lines
  where
    addY y = map (\(x,cell) -> ((x,y), cell))

                               
showArray :: Array Position Cell -> String
showArray array =
  unlines $
  [ [ charFromCell (array!(x,y))
    | x <- [1..size]
    ]
  | y <- [1..size]
  ]



neighbors :: Position -> [ Position ]
neighbors (x,y) =
  [ (u,w)
  | u <- [x-1..x+1],
    w <- [y-1..y+1],
    1 <= u && u <= size,
    1 <= w && w <= size,
    (u,w) /= (x,y)
  ]


countNeighboring :: Array Position Cell -> Cell -> Position -> Int
countNeighboring array this =
    length
  . filter (== this)
  . map (array!)
  . neighbors 


nextCell :: Array Position Cell -> Position -> (Position, Cell)
nextCell array position =
  (position, content)
  where
    content = evolve (array!position)
    evolve Open =
      if countNeighboring array Tree position >= 3 then Tree else Open
    evolve Tree =
      if countNeighboring array Lumberyard position >= 3 then Lumberyard else Tree
    evolve Lumberyard =
      if countNeighboring array Lumberyard position >= 1
         && countNeighboring array Tree position >= 1 then Lumberyard
      else Open


nextArray :: Array Position Cell -> Array Position Cell           
nextArray array =
  Arr.array (minPosition, maxPosition)
  . map (nextCell array)
  $ allPositions


history :: String -> [ Array Position Cell]
history =
  iterate nextArray . readArray 


count :: Array Position Cell -> Cell -> Int
count array cell =
  length
  . filter (== cell)
  $ Arr.elems array
             

resources :: Array Position Cell -> Int
resources array = 
  count array Tree * count array Lumberyard

  
processPart1 :: String -> String
processPart1 input =
  show $ resources array
  where
    array = history input !! 10



findRepetition :: Ord a => [(Int,a)] -> (Int, Int)
findRepetition values =
  go M.empty values
  where
    go map ((index,value):values) =
      case M.lookup value map of
        Nothing -> go (M.insert value index map) values
        Just oldIndex -> (oldIndex, index)
      

  
billion = 1000000000

processPart2 :: String -> String
processPart2 input =
  show $ resources array
  where
    (i1,i2) = findRepetition . zip [0..] . history $ input
    len = i2 - i1
    n = ((billion - i1) `rem` len) + i1
    array = history input !! n
