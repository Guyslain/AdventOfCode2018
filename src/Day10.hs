{-# LANGUAGE OverloadedStrings #-}

module Day10 (processPart1) where


import Text.Trifecta (parseString, Parser, Result (Success))
import Text.Parser.Token (symbol, textSymbol, integer)

data Vector = Vec
  { x :: Integer
  , y :: Integer
  } deriving Eq

plus :: Vector -> Vector -> Vector
vec1 `plus` vec2 =
  Vec (x vec1 + x vec2) (y vec1 + y vec2)

times :: Integer -> Vector -> Vector
scalar `times` vector =
  Vec (scalar * x vector) (scalar * y vector)
  
data Particle = Particle
  { position :: Vector
  , velocity :: Vector
  }

particleParser :: Parser Particle
particleParser = do
  _ <- textSymbol "position="
  position <- vectorParser
  _ <- textSymbol "velocity="
  velocity <- vectorParser
  return $ Particle position velocity

vectorParser :: Parser Vector
vectorParser = do
  _ <- symbol "<"
  x <- integer
  _ <- symbol ","
  y <- integer
  _ <- symbol ">"
  return $ Vec x y

parseParticle :: String -> Particle
parseParticle input =
  case parseString particleParser mempty input of
    Success particle -> particle


findVerticalClosestTime :: Particle -> Particle -> Integer
findVerticalClosestTime p1 p2 =
  if startDist * dy < 0 then 0
  else startDist `div` dy
  where
    startDist = y (position p2) - y (position p1)
    dy = y (velocity p1) - y (velocity p2)


evolve :: Particle -> Integer -> Particle
evolve particle time =
  Particle
    (position particle `plus` (time `times` velocity particle))
    (velocity particle)


showParticles :: [ Particle ] -> String
showParticles particles =
  unlines
  . map toLine
  $  [ yMin .. yMax ]
  where
    positions = map position particles
    xMin = minimum . map x $ positions
    yMin = minimum . map y $ positions
    xMax = maximum . map x $ positions
    yMax = maximum . map y $ positions
    toSymbol vec = if any ((==) vec) positions then '*' else ' '
    toLine y = map toSymbol [ Vec x y | x <- [xMin .. xMax] ]


showConfigAtTime :: [ Particle ] -> Integer -> String
showConfigAtTime particles t =
  showParticles . map (flip evolve t) $ particles

findTimeCandidate :: [ Particle ] -> Integer
findTimeCandidate (p1:p2:others) =
  findVerticalClosestTime p1 p2



processPart1 :: String -> String
processPart1 input =
  concat
    [ "\n" ++ show t ++ " =================\n" ++ showConfigAtTime particles t
    | t <- [time - 2 .. time + 2]
    ]
  where 
    particles = map parseParticle . lines $ input
    time = findTimeCandidate particles :: Integer
    
