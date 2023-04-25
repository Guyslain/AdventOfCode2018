{-# LANGUAGE OverloadedStrings #-}

module Day24 (processPart1, processPart2) where

import Prelude hiding (id)

import Text.Trifecta (parseString, Result(Success,Failure), ErrInfo, Parser)
import Text.Parser.Token (whiteSpace, integer, symbol, symbolic, textSymbol)
import Text.Parser.Combinators (some, many, eof, try, sepBy, choice, between)
import Text.Parser.Char (oneOf)
import Control.Applicative ((<|>), (<*>), (*>), (<*))

import Data.Text (Text, pack, unpack)
import Data.Either (partitionEithers)
import Data.List (intercalate, elem, delete, find, sortBy, maximumBy)
import Data.Maybe (fromMaybe)


data AttackType
  = Slashing
  | Fire
  | Radiation
  | Cold
  | Bludgeoning
    deriving Eq

instance Show AttackType where
  show attackType = 
    case lookup attackType attackTypeWords of
      Just word -> unpack word
      
    
attackTypeWords :: [ (AttackType, Text) ]
attackTypeWords =
  [ (Slashing, "slashing"),
    (Fire, "fire"),
    (Radiation, "radiation"),
    (Cold, "cold"),
    (Bludgeoning, "bludgeoning")
  ]
  

parseAttackType :: Parser AttackType
parseAttackType =
  choice $ map toParser attackTypeWords
  where
    toParser (attackType, text) =
      const attackType <$> try (textSymbol text)

data Unit
  = Unit
    { hitPoints :: Integer
    , attackDamage :: Integer
    , attackType :: AttackType
    , initiative :: Integer
    , weaknesses :: [ AttackType ]
    , immunities :: [ AttackType ]
    }

instance Show Unit where
  show (Unit hitPoints attackDamage attackType initiative weaknesses immunities) =
    show hitPoints ++ " hit points "
    ++ capacities
    ++ "with an attack that does "
    ++ show attackDamage ++ " " ++ show attackType
    ++ " damage at initiative " ++ show initiative
    where
      capacities =
        case (weaknesses, immunities) of
          ([],[]) -> ""
          (w,[]) -> "(" ++ weak ++ ") "
          ([],i) -> "(" ++ immune ++ ") "
          (w,i) -> "(" ++ weak ++ "; " ++ immune ++ ") "
      weak =
        "weak to " ++ intercalate ", " (map show weaknesses)
      immune =
        "immune to " ++ intercalate ", " (map show immunities)
        

parseWeaknesses :: Parser [ AttackType ]
parseWeaknesses =
  (try $ textSymbol "weak to") *> parseAttackType `sepBy` (try $ symbol ",")

parseImmunities :: Parser [ AttackType ]
parseImmunities = 
  (try $ textSymbol "immune to") *> parseAttackType `sepBy` (try $ symbol ",")

  


parseCapacities :: Parser ([AttackType], [AttackType])
parseCapacities =
  nonEmpty <|> pure ([],[])
  where 
    nonEmpty =
      between (try $ symbol "(") (symbol ")")
      $ parseWFirst <|> parseIFirst
    parseWFirst = pair <$> parseWeaknesses <*> parseISecond
    parseIFirst = riap <$> parseImmunities <*> parseWSecond
    parseISecond =
      ((try $ symbol ";") *> parseImmunities) <|> pure []
    parseWSecond =
      ((try $ symbol ";") *> parseWeaknesses) <|> pure []
    pair w i = (w,i)
    riap i w = (w,i)

parseUnit ::  Parser Unit
parseUnit = do
  hitPoints <- fromIntegral <$> integer
  _<- textSymbol "hit points"
  (weaknesses, immunities) <- parseCapacities
  _ <- textSymbol "with an attack that does"
  attackDamage <- fromIntegral <$> integer
  attackType <- parseAttackType
  _ <- textSymbol "damage at initiative"
  initiative <- fromIntegral <$> integer
  return $ Unit
    hitPoints
    attackDamage
    attackType
    initiative
    weaknesses
    immunities

    
data Side = ImmuneSystem | Infection deriving Eq    


data Group
  = Group
    { nbUnits :: Integer
    , unit :: Unit
    , side :: Side
    , id :: Int
    }

instance Eq Group where
  group1 == group2 = id group1 == id group2

instance Show Group where
  show (Group nbUnits unit side id) =
    case nbUnits of 
      0 -> "empty group"
      1 -> "1 unit with " ++ show unit
      many -> show nbUnits ++ " units each with " ++ show unit


parseGroup :: Int -> Side -> Parser Group
parseGroup id side =
  toGroup <$> nbUnits <*> textSymbol "units each with" <*> parseUnit
  where
    toGroup nbUnits _ unit = Group nbUnits unit side id
    nbUnits = fromIntegral <$> integer


readGroup :: Int -> Side -> String -> Group
readGroup id side line =
  case parseString (parseGroup id side) mempty line of
    Success group -> group


type State = [ Group ]

readInput :: String -> State
readInput input =
  (map (fromString ImmuneSystem) $ zip [0..] immunes)
  ++ (map (fromString Infection) $ zip [length immunes..] infects)
  where
    fromString side (id,line) = readGroup id side line
    immunes = tail . takeWhile (not . null) $ ls
    infects = drop 2 . dropWhile (not . null) $ ls
    ls = lines input


showGroups :: [ Group ] -> [ String ]
showGroups groups =
  ["Immune System"] ++ (map show $ immunes) ++ [""]
  ++ ["Infection"] ++ (map show $ infects) ++ [""]
  where
    immunes = filter ((== ImmuneSystem) . side) groups
    infects = filter ((== Infection) . side) groups


    
effectivePower :: Group -> Integer
effectivePower group = nbUnits group * (attackDamage . unit $ group)


damagesTo :: Group -> Group -> Integer
damagesTo attacker defender
  | side attacker == side defender = 0
  | attackType unitAttack `elem` immunities unitDefend = 0
  | attackType unitAttack `elem` weaknesses unitDefend = 2 * effectivePower attacker
  | otherwise = effectivePower attacker
  where
    unitAttack = unit attacker
    unitDefend = unit defender

tieBreaking :: Ordering -> Ordering -> Ordering
tieBreaking EQ res = res
tieBreaking res _ = res

elseCompare :: (a -> a -> Ordering) -> (a -> a -> Ordering) -> (a -> a -> Ordering)
elseCompare compare1 compare2 item1 item2 =
  compare1 item1 item2 `tieBreaking` compare2 item1 item2
  

compareTarget :: Group -> Group -> Group -> Ordering
compareTarget attacker =
    (compare `on` (attacker `damagesTo`))
    `elseCompare` (compare `on` effectivePower)
    `elseCompare` (compare `on` (initiative . unit))

on :: (a -> a -> c) -> (b -> a) -> (b -> b -> c)
f `on` key = \ u v -> f (key u) (key v)


chooseTarget :: Group -> [ Group ] -> Maybe Group
chooseTarget attacker enemies =
  if null targets then Nothing
  else Just $ maximumBy (compareTarget attacker) targets
  where
    targets = filter nonNullAttack enemies
    nonNullAttack enemy = attacker `damagesTo` enemy /= 0


targetSelection :: State -> [ (Group,Group) ]
targetSelection groups =
  fst . foldr select ([], groups) . sortBy (compare `on` effectivePower) $ groups
  where
    select attacker (attacks, possibleTargets) =
      case chooseTarget attacker possibleTargets of
        Nothing -> (attacks, possibleTargets)
        Just defender -> ((attacker,defender):attacks, delete defender possibleTargets)


resolveAttack :: (Group, Group) -> State -> State
resolveAttack (attack, defend) groups =
  fromMaybe groups $ do
  attacker <- find (== attack) groups
  if nbUnits attacker == 0 then return groups
  else do
    defender <- find (== defend) groups  
    let otherGroups = delete defender groups
        newDefender = defender { nbUnits = newNbUnits }
        defUnit = unit defender
        damages = attacker `damagesTo` defender
        nbKilled = damages `div` hitPoints defUnit
        newNbUnits = nbUnits defender - nbKilled
    if newNbUnits <= 0 then return otherGroups
    else return (newDefender:otherGroups)


sortAttacker :: [ (Group, Group) ] -> [ (Group, Group) ]
sortAttacker =
  sortBy (compare `on` (initiative . unit . fst))
    
fight :: State -> State
fight state =
  foldr resolveAttack state matching
  where
    matching = sortAttacker $ targetSelection state


isOver :: State -> Bool
isOver state =
  all (== ImmuneSystem) sides
  || all (== Infection) sides
  || null (targetSelection state)
  where
    sides = map side state


game :: State -> State
game state =
  if isOver state then state
  else game $ fight state

winningSide :: State -> Maybe Side
winningSide state
  | all (== ImmuneSystem) . map side $ game state = Just ImmuneSystem
  | all (== Infection) . map side $ game state = Just Infection
  | otherwise = Nothing



showMatching :: [ (Group, Group) ] -> [ String ]
showMatching =
  map showPair
  where
    showPair (attacker,defender) = show attacker ++ " attacks " ++ show defender

verboseGame :: State -> String
verboseGame =
  go [] 
  where
    answer = show . sum . map nbUnits
    go output state
      | isOver state =
        unlines
        . concat
        . reverse
        $ ([answer state] : ["\n"] : showGroups state : output)
      | otherwise =
        let matching = sortAttacker $ targetSelection state in
        let outcome = fight state in
        go (["\n\n"] : showMatching matching : showGroups state : output) outcome
          

    

processPart1 :: String -> String
processPart1 = verboseGame . readInput
  -- show . sum . map nbUnits $ game groups
  -- where
  --   groups = readInput input

boostUnit :: Integer -> Unit -> Unit
boostUnit boost unit = unit { attackDamage = attackDamage unit + boost }

boostGroup :: Integer -> Group -> Group
boostGroup boost group
  | side group == ImmuneSystem = group { unit = boostUnit boost (unit group) }
  | otherwise = group

boostState :: Integer -> State -> State
boostState boost = map (boostGroup boost) 

findOptimalBoost :: Integer -> Integer -> State -> Integer
findOptimalBoost lower upper state
  | lower + 1 == upper = upper
  | winningSide (boostState middle state) == Just ImmuneSystem =
    findOptimalBoost lower middle state
  | otherwise =
    findOptimalBoost middle upper state
  where
    middle = (lower + upper) `div` 2 


processPart2 :: String -> String
processPart2 input =
  verboseGame winningState
  where
    initState = readInput input
    boost = findOptimalBoost 0 100 initState
    winningState = boostState boost initState
