module Day14 (processPart1, processPart2) where

import Data.Array


reverseDigits :: Int -> [ Int ]
reverseDigits n
  | n < 10 = [ n ]
  | otherwise = n `mod` 10 : reverseDigits (n `div` 10)

digits :: Int -> [ Int ]
digits = reverse . reverseDigits

modulo :: Int -> Int -> Int
modulo n d =
  if m == 0 then d else m
  where
    m = n `mod` d 


spinize 0 _ = []
spinize n l = head l : spinize (n-1) (tail l)

data Position = Position
  { elf1 :: Int
  , elf2 :: Int
  , recipesCount :: Int 
  } deriving Show

startingPosition :: Position
startingPosition =
  Position 1 2 2 



listRecipes :: [Int ]
listRecipes =
  3 : 7 : (concat . map fst . iterate play $ ([], startingPosition))


play :: ([ Int ], Position) -> ([ Int ], Position)
play (_, Position pos1 pos2 recipesCount) =
  (newRecipes, newPosition)
  where
    recipe1 = recipes!pos1
    recipe2 = recipes!pos2
    newRecipes = digits $ recipe1 + recipe2
    newRecipesCount = recipesCount + length newRecipes
    newPosition =
      Position
        ((pos1 + 1 + recipe1) `modulo` newRecipesCount)
        ((pos2 + 1 + recipe2) `modulo` newRecipesCount)
        newRecipesCount

recipes :: Array Int Int
recipes =
  listArray (1, len) (spinize len listRecipes)


len = 40000000


input :: Int
input = 320851
nbDigits = 6
rounder = 1000000

  
offset :: Int
offset = 10


processPart1 :: String -> String
processPart1 _ =
  concat [ show (recipes!i) | i <- [input+1..input+offset] ]

processPart2 :: String -> String
processPart2 _ =
  show $ find 0 startValue
  where
    find i value
      | value == input = i
      | otherwise = find (i+1) ((value * 10  + recipes!(i+nbDigits+1)) `mod` rounder)
    startValue = foldl (\s d -> 10 * s + d) 0 $ take nbDigits listRecipes
