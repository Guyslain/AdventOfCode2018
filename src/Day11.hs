module Day11 (processPart1, processPart2) where

import Data.List (sortBy)
import Data.Array 

cellPowerLevel :: Int -> (Int,Int) -> Int
cellPowerLevel serialNumber (x,y) =
  (\p -> p - 5)
  . hundredDigit
  . (* rackId)
  . (+ serialNumber)
  . (* y)
  $ rackId
  where
    rackId = x + 10
    hundredDigit int = (int `mod` 1000) `div` 100


subgridPowerLevel :: Int -> Int -> (Int, Int) -> Int
subgridPowerLevel serialNumber dim (x,y) =
  sum
    [ cellPowerLevel serialNumber (x',y')
    | x' <- [x..x+dim-1], y' <- [y..y+dim-1]
    ]
    

largestPowerCell :: Int -> Int -> (Int, Int, Int, Int)
largestPowerCell dim serialNumber =
  head
  . sortBy (\(x1,y1,dim1,p1) (x2,y2,dim2,p2) -> compare p2 p1)
  $ [ (x, y, dim, subgridPowerLevel serialNumber dim (x,y))
    | x <- [1..301-dim], y <- [1..301 - dim]
    ]
  

processPart1 :: String -> String
processPart1 = show . largestPowerCell 3 . read



powerLevelArray :: Int -> Array (Int, Int) Int
powerLevelArray serialNumber =
  array
    ((1,1), (300,300))
    [ ((x,y), cellPowerLevel serialNumber (x,y))
    | x <- [1..300], y <- [1..300]
    ]



topRectanglePowerLevelArray :: Int -> Array (Int, Int) Int
topRectanglePowerLevelArray serialNumber =
  sumArray
  where
    sumArray =
      array
        ((0,0),(300,300))
        [ ((x,y), powerAt x y)
        | x <- [0..300], y <- [0..300]
        ]
    powerArray = powerLevelArray serialNumber
    powerAt 0 _ = 0
    powerAt _ 0 = 0
    powerAt n p =
      powerArray!(n,p) + sumArray!(n-1,p)
      + sumArray!(n,p-1) - sumArray!(n-1,p-1)

      

maxBy :: Ord a => (a -> a -> Ordering) -> [a] -> a
maxBy f (x:xs) =
  foldr (\ elem best -> if f elem best == GT then elem else best) x xs
  

maxSubgridPower :: Int -> (Int, Int, Int, Int)
maxSubgridPower serialNumber  =
  maxBy (\(x1,y1,d1,p1) (x2,y2,d2,p2) -> compare p1 p2)
  $ [ (x,y,d,power x y d) |
      d <- [1..300],
      x <- [1..301-d],
      y <- [1..301-d]
    ]
  where
    topRectPowerArray = topRectanglePowerLevelArray serialNumber
    power x y d =
      topRectPowerArray!(x+d-1,y+d-1) + topRectPowerArray!(x-1,y-1)
      - topRectPowerArray!(x+d-1,y-1) - topRectPowerArray!(x-1,y+d-1)


processPart2 :: String -> String
processPart2 = show . maxSubgridPower . read
