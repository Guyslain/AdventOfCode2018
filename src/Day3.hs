module Day3
    ( processPart1
    , processPart2
    ) where

import Prelude hiding (id)
import Text.Trifecta
import Text.Parser.Token
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (all)


data Rectangle = Rectangle
  { minX :: Integer,
    minY :: Integer,
    width :: Integer,
    height :: Integer
  }

data Claim = Claim
  { id :: Integer
  , rectangle :: Rectangle
  }


claimParser :: Parser Claim
claimParser = do
  _ <- symbolic '#'
  id <- integer
  _ <- symbolic '@'
  leftMargin <- integer
  _ <- symbolic ','
  topMargin <- integer
  _ <- symbolic ':'
  width <- integer
  _ <- symbolic 'x'
  height <- integer
  return $ Claim id (Rectangle leftMargin topMargin width height)

parseClaim :: String -> Claim
parseClaim line =
  case parseString claimParser mempty line of
    Success claim -> claim
    


cutpoints :: (Claim -> [Integer]) -> [Claim] -> Set.Set Integer
cutpoints extractCutpoints=
  foldr Set.insert Set.empty
  . concat 
  . map extractCutpoints

horizontalCutpoints :: [Claim] -> Set.Set Integer
horizontalCutpoints = cutpoints extractHorizontalCutpoints
  where    
  extractHorizontalCutpoints claim =
    [ (minX $ rectangle claim),
      (minX $ rectangle claim) + (width $ rectangle claim)
    ]


verticalCutpoints :: [Claim] -> Set.Set Integer
verticalCutpoints = cutpoints extractVerticalCutpoints
  where
  extractVerticalCutpoints claim =
    [ (minY $ rectangle claim),
      (minY $ rectangle claim) + (height $ rectangle claim)
    ]


maxX :: [Claim] -> Integer
maxX =
  foldr max 0 . map (getMaxX . rectangle)
  where getMaxX rectangle = minX rectangle + width rectangle
        
maxY :: [Claim] -> Integer
maxY =
  foldr max 0 . map (getMaxY . rectangle)
  where getMaxY rectangle = minY rectangle + height rectangle

containsPoint :: (Integer, Integer) -> Rectangle -> Bool 
containsPoint (x,y) (Rectangle minX minY width height) =
  minX <= x && x < minX + width
  && minY <= y && y < minY + height
  
  
countOccurences :: [Claim] -> (Integer, Integer) -> Int
countOccurences claims (x,y) =
  length
  . filter (containsPoint (x,y))
  . map rectangle
  $ claims

countOverCovered :: [Claim]  -> Int
countOverCovered claims =
  length
  . filter (>= 2)
  . map (countOccurences claims)
  $ [ (i,j) | i <- [0..x], j <- [0..y] ]
  where
    x = maxX claims
    y = maxY claims

    

processPart1 :: String -> String
processPart1 = show . countOverCovered . map parseClaim . lines 


areOverlapping :: Rectangle -> Rectangle -> Bool
areOverlapping (Rectangle x1 y1 w1 h1) (Rectangle x2 y2 w2 h2) =
  intervalOverlaps (x1, x1 + w1) (x2, x2 + w2)
  && intervalOverlaps (y1, y1 + h1) (y2, y2 + h2)

intervalOverlaps :: (Integer, Integer) -> (Integer, Integer) -> Bool
intervalOverlaps (l1,u1) (l2,u2) =
  not (u1 <= l2 || u2 <= l1)

overlapsNothing :: [Claim] -> Claim -> Bool
overlapsNothing claims claim =
  all
    (\c -> id c == id claim
           || not (areOverlapping (rectangle claim) (rectangle c)))
    claims
  
nonOverlappingClaims :: [Claim] -> [Claim]
nonOverlappingClaims claims =
  filter (overlapsNothing claims) claims

processPart2 :: String -> String 
processPart2 =
  show
  . map id
  . nonOverlappingClaims
  . map parseClaim
  . lines
