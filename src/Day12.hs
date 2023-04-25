{-# LANGUAGE OverloadedStrings #-}

module Day12 (processPart1, processPart2) where


import Text.Trifecta (parseString, Result(Success,Failure), ErrInfo, Parser)
import Text.Parser.Token (whiteSpace, symbol, symbolic, textSymbol)
import Text.Parser.Combinators (some, eof)
import Text.Parser.Char (oneOf)
import Control.Applicative ((<|>))
import Data.List (find)
import qualified Data.List as List 

data Rule = Rule { input :: [Bool], output :: Bool } deriving Show

data Tape a = Tape
  { left :: [ a ]
  , head :: a
  , right :: [ a ]
  }

instance Show a => Show (Tape a) where
  show = show . sublist (-10) 10

instance Functor Tape where
  fmap f (Tape left head right) =
    Tape (map f left) (f head) (map f right)

fromList :: a -> [a] -> Tape a
fromList pad [] = Tape (repeat pad) pad (repeat pad)
fromList pad (head:right) = Tape (repeat pad) head (right ++ repeat pad)

moveLeft :: Tape a -> Tape a
moveLeft (Tape (a:left) b right) = Tape left a (b:right)
moveRight :: Tape a -> Tape a
moveRight (Tape left a (b:right)) = Tape (a:left) b right

sublist :: Int -> Int -> Tape a -> [ a ]
sublist 1 len (Tape left head right) = take len right
sublist start stop tape
  | start < 1 = sublist (start+1) (stop+1) (moveLeft tape)
  | start > 1 = sublist (start-1) (stop-1) (moveRight tape)

generate :: a -> (a -> a) -> (a -> a) -> Tape a
generate head fleft fright=
  Tape (iterate fleft (fleft head)) head (iterate fright (fright head))

extends :: Tape a -> Tape (Tape a)
extends tape = 
  generate tape moveLeft moveRight

combine :: Tape a -> Tape b -> Tape (a,b)
combine (Tape left1 head1 right1) (Tape left2 head2 right2) =
  Tape
    (List.zip left1 left2)
    (head1, head2)
    (List.zip right1 right2)

index :: Tape a -> Tape (a, Int)
index tape =
  combine tape (generate 0 (\i -> i - 1) (+ 1))

evolve :: [ Rule ] -> Tape Bool -> Tape Bool
evolve rules =
    fmap applyRules
  . fmap (sublist (-2) 2)
  . extends
  where
    applyRules pattern =
      toOutput $ find (\(Rule input output) -> input == pattern) rules
    toOutput (Just (Rule input output)) = output
    toOutput Nothing = False



generations :: [ Rule ] -> Tape Bool -> [ Tape Bool ]
generations rules = iterate (evolve rules)
    


evalTape :: Int -> Int -> Tape Bool -> Int
evalTape from to =
  sum
  . map snd 
  . filter fst 
  . sublist from to
  . index

boolParser :: Parser Bool
boolParser = do
  char <- oneOf "#."
  return $ char == '#'
  
boolListParser :: Parser [Bool]
boolListParser = do
  list <- some boolParser
  _ <- whiteSpace
  return list

  
initialStateParser :: Parser (Tape Bool)
initialStateParser = do
  _ <- textSymbol "initial state:"
  symbols <- boolListParser
  return . fromList False $ symbols

ruleParser :: Parser Rule
ruleParser = do
  input <- boolListParser
  _ <- symbol "=>"
  output <- boolParser
  _ <- whiteSpace
  return $ Rule input output

inputParser :: Parser (Tape Bool, [Rule])
inputParser = do
  _ <- whiteSpace
  state <- initialStateParser
  rules <- some ruleParser
  _ <- eof
  return (state, rules)


parseInput :: String -> Either (Tape Bool, [ Rule ]) ErrInfo
parseInput input =
  case parseString inputParser mempty input of
    Success s -> Left s
    Failure info -> Right info
    




processPart1 :: String -> String 
processPart1 input =
  case parseInput input  of
    Left (tape, rules) ->
      show
      . evalTape (-20) 200
      . (!! 20)
      . generations rules
      $ tape
    Right err -> show err 


showTape :: Tape Bool -> String
showTape tape =
  (show . evalTape (-20) 200 $ tape)
  ++ " " ++ (map toSymbol . sublist (-2) 200 $ tape)
  where
    toSymbol True = '#'
    toSymbol False = '.'


processPart2 :: String -> String 
processPart2 input =
  unlines
  . map showTape
  . take 100
  . generations rules
  $ tape 
  where
    Left (tape, rules) = parseInput input

