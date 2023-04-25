{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Day16 (processPart1, processPart2) where

import Prelude hiding (lookup)

import Data.Bits ((.&.),(.|.))
import Data.List (intercalate, intersect, delete, sortOn)

import Control.Monad
import Control.Monad.State.Class (MonadState, get, put)
import Control.Monad.Except (MonadError, throwError, catchError)

type Register = Int
type RegisterValues = (Int, Int, Int, Int)


load :: (MonadState RegisterValues m, MonadError String m) =>
        Register -> m Int
load 0 = (\ (zero,_,_,_) -> zero) <$> get
load 1 = (\ (_,one,_,_) -> one) <$> get
load 2 = (\ (_,_,two,_) -> two) <$> get
load 3 = (\ (_,_,_,three) -> three) <$> get
load n = throwError $ "load " ++ show n


save :: (MonadState RegisterValues m, MonadError String m) =>
        Register -> Int -> m ()
save i value = do
  (a,b,c,d) <- get
  case i of
    0 -> put (value,b,c,d)
    1 -> put (a,value,c,d)
    2 -> put (a,b,value,d)
    3 -> put (a,b,c,value)
    n -> throwError $ "save " ++ show n
  

type Operation m = Int -> Int -> Int -> m ()
data OpCode m
  = OpCode
    { name :: String
    , instruction :: Operation m
    }

instance Show (OpCode m) where
  show = name

instance Eq (OpCode m) where
  op1 == op2 = name op1 == name op2


biOperatorR :: (MonadState RegisterValues m, MonadError String m) =>
              (Int -> Int -> Int) -> Operation m
biOperatorR op = \ regA regB regC -> do
  a <- load regA
  b <- load regB
  save regC (a `op` b)

biOperatorI :: (MonadState RegisterValues m, MonadError String m) =>
              (Int -> Int -> Int) -> Operation m
biOperatorI op = \ regA valB regC -> do
  a <- load regA
  save regC (a `op` valB)


operators ::[ Int -> Int -> Int ]
operators = [(+), (*), (.&.), (.|.)]

arithROperators :: (MonadState RegisterValues m, MonadError String m) =>
                   [ OpCode m ]
arithROperators =
    zipWith OpCode ["addr", "mulr", "banr", "borr" ]
  . map biOperatorR
  $ operators

arithIOperators :: (MonadState RegisterValues m, MonadError String m) =>
                   [ OpCode m ]
arithIOperators =
  zipWith OpCode ["addi", "muli", "bani", "bori" ]
  . map biOperatorI
  $ operators


setR :: (MonadState RegisterValues m, MonadError String m) =>
        OpCode m
setR = OpCode "setr" op
  where op regA _ regC = load regA >>= save regC

setI :: (MonadState RegisterValues m, MonadError String m) =>
        OpCode m
setI = OpCode "seti" op
  where op a _ regC = save regC a


comparator :: (MonadState RegisterValues m, MonadError String m) =>
  (Int -> m Int) -> (Int -> m Int) -> (Int -> Int -> Bool) -> Operation m
comparator useA useB comp = \ a b c -> do
  valA <- useA a
  valB <- useB b
  save c (if valA `comp` valB then 1 else 0)

comparators :: (MonadState RegisterValues m, MonadError String m) =>
               [ OpCode m ]
comparators=
  [ OpCode "gtir" (comparator pure load (>))
  , OpCode "gtri" (comparator load pure (>))
  , OpCode "gtrr" (comparator load load (>))
  , OpCode "eqir" (comparator pure load (==))
  , OpCode "eqri" (comparator load pure (==))
  , OpCode "eqrr" (comparator load load (==))
  ]
 
allOpCodes :: (MonadState RegisterValues m, MonadError String m) =>
                [ OpCode m ]
allOpCodes =
  arithIOperators ++ arithROperators ++ [setR, setI] ++ comparators





  
type EncodedInstruction = (Int, Int, Int, Int)

data Sample =
  Sample
  { before :: RegisterValues
  , code :: EncodedInstruction
  , after :: RegisterValues
  }
  deriving Show

readSample :: String -> String -> String -> Sample
readSample lineBefore lineInstr lineAfter =
  Sample
  { before = toTuple . read $ drop 8 lineBefore
  , code = toTuple . map read $ words lineInstr
  , after = toTuple . read $ drop 6 lineAfter
  }
  where
    toTuple [a,b,c,d] = (a,b,c,d)
    
extractSamples :: [ Sample ] -> [String] -> ([ Sample ], [String])
extractSamples samples ("":"":"":remains) = (samples, remains)
extractSamples samples (lineBefore:lineInstr:lineAfter:"":remains) =
  extractSamples (readSample lineBefore lineInstr lineAfter : samples) remains
extractSamples samples remains = (samples, remains)



data Instruction m
     = Instruction (OpCode m) Int Int Int

instance Show (Instruction m) where
  show (Instruction code a b c) = intercalate " " $ [show code, show a, show b, show c]
  

data Program out
  = Program { run :: RegisterValues -> Either String (RegisterValues, out) }


instance Functor Program where
  fmap f instr = Program $ \ registers ->
    case run instr registers of
      Left error -> Left error
      Right (newRegisters, result) -> Right (newRegisters, f result)

instance Applicative Program where
  pure value = Program $ \ registers -> Right (registers, value)
  getF <*> x = Program $ \ registers ->
    case run getF registers of
      Left error -> Left error
      Right (newRegisters, f) -> run (fmap f x) newRegisters

instance Monad Program where
  return = pure
  getA >>= f = Program $ \ register ->
    case run getA register of
      Left error -> Left error
      Right (newRegisters, a) -> run (f a) newRegisters

instance MonadState RegisterValues Program where
  get = Program $ \ registers -> Right (registers,registers)
  put newRegisters = Program $ \ registers -> Right (newRegisters, ())

instance MonadError String Program where
  throwError err = Program $ \ registers -> Left err
  catchError effect resolver = Program $ \ registers ->
    case run effect registers of
      Right (newRegisters, value) -> Right (newRegisters, value)
      Left err -> run (resolver err) registers


mkSingleInstructionProgram :: Instruction Program -> Program ()
mkSingleInstructionProgram (Instruction opCode a b c) =
  instruction opCode a b c

mkProgram :: [ Instruction Program ] -> Program ()
mkProgram instructions =
  forM_ instructions mkSingleInstructionProgram


isCompatibleOpCode :: Sample -> OpCode Program -> Bool
isCompatibleOpCode sample opCode =
  case run program $ before sample of
    Left error -> False
    Right (result,()) -> result == after sample
  where
    program = mkSingleInstructionProgram instr
    instr = Instruction opCode b c d
    (a,b,c,d) = code sample


findOpCodes ::
  Sample -> [ OpCode Program ]
findOpCodes sample =
  filter (isCompatibleOpCode sample) allOpCodes 
  
  
allSamples :: String -> [ Sample ]
allSamples = reverse . fst . extractSamples [] . lines 


processPart1 :: String -> String
processPart1 input =
  show
  . length
  . filter ((>= 3) . length)
  . map findOpCodes
  . allSamples
  $ input



possibleOpCodes :: [ Sample ] -> Int -> [ OpCode Program ]
possibleOpCodes allSamples currentCode =
  foldr intersect allOpCodes
  . map findOpCodes
  $ samples
  where
    filterOpCodes opCodes sampleOpCode =
      filter (\op -> op `elem` sampleOpCode) opCodes
    samples = filter hasCurrentCode allSamples
    hasCurrentCode sample =
      first (code sample) == currentCode
    first (a,_,_,_) = a


constraints :: [ Sample ] -> [ (Int, [ OpCode Program] ) ]
constraints samples =
  [ (index, possibleOpCodes samples index)
  | index <- [0..15]
  ]



matchings :: (Eq a, Eq b) => [ (a, [b]) ]-> [ [ (a,b) ] ]
matchings =
  chooseFirst . sortOn (length . snd)
  where
    chooseFirst [] = [[]]
    chooseFirst ((a,bs):others) = do
      b <- bs
      matching <- matchings $ remove b others
      return ((a,b):matching)
    remove b = map (\(a,bs) -> (a, delete b bs)) 
    



decodeInstruction :: (Int -> OpCode Program) -> String -> Instruction Program
decodeInstruction toOpCode =
  toInstruction
  . map read
  . words
  where
    toInstruction [a,b,c,d] =
      Instruction (toOpCode a) b c d

allInstructions :: (Int -> OpCode Program) -> String -> [ Instruction Program ]
allInstructions toOpCode =
    map (decodeInstruction toOpCode)
  . filter (not . null)
  . dropWhile (not . null)
  . snd
  . extractSamples []
  . lines


lookup :: Eq a => a -> [ (a,b) ] -> b
lookup a ((x,b):pairs)
  | a == x = b
  | otherwise = lookup a pairs

processPart2 :: String -> String
processPart2 input =
  case run program (0,0,0,0) of
    Left error -> error
    Right ((a,b,c,d),()) -> show a
  where
    program =
      mkProgram
      . allInstructions (\code -> lookup code matching)
      $ input
    matching =
      head
      . matchings
      . constraints
      . allSamples
      $ input
