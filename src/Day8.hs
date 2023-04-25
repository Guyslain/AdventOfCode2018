module Day8 (processPart1, processPart2) where

import Data.List (sum)

data Tree =
  Tree
  { metadata :: [ Int ]
  , children :: [ Tree ]
  }
  deriving Show
                
            
  

readTree :: [ Int ] -> (Tree, [ Int ])
readTree (childCount:metadataCount:input) =
  ( Tree
      metadata
      children,
      remains
  )
  where
    (children, inputLeft)  = readTrees childCount input
    metadata = take metadataCount inputLeft
    remains = drop metadataCount inputLeft

readTrees :: Int -> [ Int ] -> ([ Tree ], [ Int ])
readTrees 0 input = ([], input)
readTrees k input =
  (tree:trees, leftOver)
  where
    (tree, remains) = readTree input
    (trees, leftOver) = readTrees (k-1) remains

sumTree :: Tree -> Int
sumTree tree =
  sum (metadata tree)
  + sum (map sumTree $ children tree) 


parseTree :: String -> Tree
parseTree =
  fst
  . readTree
  . map (read :: String -> Int)
  . words


  

processPart1 :: String -> String
processPart1 =
  show
  . sumTree
  . parseTree


indexValue :: Tree -> Int -> Int
indexValue tree index
  | index <= length (children tree) =
      value (children tree !! (index - 1))
  | otherwise = 0

value :: Tree -> Int
value tree
  | null (children tree) = sum $ metadata tree
  | otherwise =
    sum
    . map (indexValue tree)
    $ metadata tree
    
    
  
  

processPart2 :: String ->  String
processPart2 =
  show
  . value
  . parseTree
