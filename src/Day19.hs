{-# LANGUAGE FlexibleInstances #-}
module Day19 (processPart1, processPart2) where

import Prelude hiding (log)

import Data.Bits ((.&.), (.|.))
import Control.Monad
import Control.Monad.Reader
import Control.Monad.ST

import Data.Array.IArray (Array, IArray,(!))
import qualified Data.Array.IArray as Arr

import qualified Data.Array.MArray as MArr
import Data.Array.ST (STArray)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)




type Register = Int


class Monad m => VirtualMachineM m where
  loadRegister :: Register -> m Int
  saveRegister :: Register -> Int -> m ()
  nbRegisters :: m Int
  getInstructionPointer :: m Int
  setInstructionPointer :: Int -> m ()
  getInstructionPointerRegister :: m Int 
  halt :: m ()
  isRunning :: m Bool
  log :: String -> m ()


snapshot :: VirtualMachineM m => m ()    
snapshot = do
  ip <- getInstructionPointer
  nbReg <- nbRegisters
  registers <- forM [0..nbReg-1] loadRegister
  log $ "ip: " ++ show ip ++ " registers: " ++ show registers
  
arithR :: VirtualMachineM m =>
          (Int -> Int -> Int) -> Register -> Register -> Register -> m ()
arithR op regA regB regC = do
  valA <- loadRegister regA
  valB <- loadRegister regB
  saveRegister regC $ op valA valB

arithI :: VirtualMachineM m => (Int -> Int -> Int) -> OpCode m
arithI op regA valB regC = do
  valA <- loadRegister regA
  saveRegister regC $ op valA valB

type OpCode m = Register -> Int -> Register -> m ()

addR, addI, mulR, mulI, banR, banI, borR, borI :: VirtualMachineM m => OpCode m
addR = arithR (+)
addI = arithI (+)
mulR = arithR (*)
mulI = arithI (*)
banR = arithR (.&.)
banI = arithI (.&.)
borR = arithR (.|.)
borI = arithI (.|.)

setR :: VirtualMachineM m => OpCode m
setR regA _ regC = do
  valA <- loadRegister regA
  saveRegister regC valA

setI :: VirtualMachineM m => OpCode m
setI valA _ regC = do
  saveRegister regC valA


compInstr :: VirtualMachineM m =>
   (Int -> Int -> Bool) -> (Int -> m Int) -> (Int -> m Int) -> OpCode m
compInstr op getA getB a b regC = do
  valA <- getA a
  valB <- getB b
  saveRegister regC $ if op valA valB then 1 else 0


gtIR, gtRI, gtRR, eqRI, eqIR, eqRR :: VirtualMachineM m => OpCode m
gtIR = compInstr (>) return loadRegister
gtRI = compInstr (>) loadRegister return
gtRR = compInstr (>) loadRegister loadRegister
eqIR = compInstr (==) return loadRegister
eqRI = compInstr (==) loadRegister return
eqRR = compInstr (==) loadRegister loadRegister


data Instruction m
  = Op (OpCode m) Register Int Register
    


data Program m = Program
  { nbInstructions :: Int
  , getInstruction :: Int -> Maybe (Instruction m)
  }



evaluateInstruction :: VirtualMachineM m => Maybe (Instruction m) -> m ()
evaluateInstruction Nothing = do
  halt
evaluateInstruction (Just (Op opCode regA valB regC)) = do
  opCode regA valB regC


incrementInstructionPointer :: VirtualMachineM m => m ()
incrementInstructionPointer = do
  ip <- getInstructionPointer  
  setInstructionPointer (ip+1)


runNextInstruction :: VirtualMachineM m => Program m -> m ()
runNextInstruction program = do
  ipReg <- getInstructionPointerRegister
  ip <- getInstructionPointer
  saveRegister ipReg ip
  evaluateInstruction (getInstruction program ip)
  ipNew <- loadRegister ipReg
  setInstructionPointer ipNew
  incrementInstructionPointer


whileM :: Monad m => m Bool -> m () -> m ()
whileM condition action = do
  continue <- condition
  if continue
  then do
    action
    whileM condition action
  else
    return ()


run :: VirtualMachineM m => Program m -> m ()
run program = do
  snapshot
  whileM isRunning $ do
    runNextInstruction program
    snapshot



readInstruction :: VirtualMachineM m => String -> Instruction m
readInstruction line =
  case words line of
    [instr, a, b, c] -> Op (toOpCode instr) (read a) (read b) (read c)
  where
    toOpCode "addr" = addR
    toOpCode "addi" = addI
    toOpCode "mulr" = mulR
    toOpCode "muli" = mulI
    toOpCode "banr" = banR
    toOpCode "bani" = banI
    toOpCode "borr" = borR
    toOpCode "bori" = borI
    toOpCode "setr" = setR
    toOpCode "seti" = setI
    toOpCode "gtir" = gtIR
    toOpCode "gtri" = gtRI
    toOpCode "gtrr" = gtRR
    toOpCode "eqir" = eqIR
    toOpCode "eqri" = eqRI
    toOpCode "eqrr" = eqRR





readProgram :: VirtualMachineM m => String -> Program m
readProgram input =
  Program
  { nbInstructions = maxIndex - minIndex + 1
  , getInstruction = getInstr
  }
  where
    (ipreg : instrLines) = lines input
    array = makeProgramArray instrLines
    getInstr ip =
      if minIndex <= ip && ip <= maxIndex then Just $ array!ip
      else Nothing                                       
    (minIndex, maxIndex) = Arr.bounds array

readInstructionPointerRegister :: String -> Int
readInstructionPointerRegister =
  read . head . tail . words . head . lines
  

makeProgramArray :: VirtualMachineM m => [ String ] ->  Array Int (Instruction m)
makeProgramArray lines = 
  Arr.listArray (0, length instructions - 1) instructions
  where
    instructions = map readInstruction lines 

  

data VirtualMachine s =
  VM
  { _getNbRegisters :: Int
  , _getRegisters :: STArray s Int Int
  , _getInstructionPointer :: STRef s Int
  , _getInstructionPointerRegister :: Register
  , _getIsRunning :: STRef s Bool
  , _getSnapshots :: STRef s [ String ]
  }

-- data App s r 
--   App { run :: VirtualMachine s -> ST s r }

freeze :: STArray s Int Int -> ST s (Array Int Int)
freeze array = MArr.freeze array

instance VirtualMachineM (ReaderT (VirtualMachine s) (ST s)) where
  nbRegisters = do
    _getNbRegisters <$> ask
  loadRegister reg = do
    registers <- _getRegisters <$> ask
    lift $ MArr.readArray registers reg
  saveRegister reg value = do
    registers <- _getRegisters <$> ask
    lift $ MArr.writeArray registers reg value
  getInstructionPointerRegister = do
    _getInstructionPointerRegister <$> ask
  getInstructionPointer = do
    ipRef <- _getInstructionPointer <$> ask
    lift $ readSTRef ipRef
  setInstructionPointer value = do
    ipRef <- _getInstructionPointer <$> ask
    lift $ writeSTRef ipRef value
  halt = do
    runRef <- _getIsRunning <$> ask
    lift $ writeSTRef runRef False
  isRunning = do
    runRef <- _getIsRunning <$> ask
    snapRef <- _getSnapshots <$> ask
    snapshots <- lift $ readSTRef snapRef
    isRunning <- lift $ readSTRef runRef
    return $ isRunning -- && length snapshots < 1000
  log text = do
--    return ()
    snapRef <- _getSnapshots <$> ask
    snapshots <- lift $ readSTRef snapRef
    lift $ writeSTRef snapRef (text:snapshots)


minRegister, maxRegister :: Int
minRegister = 0
maxRegister = 5
  
initialVirtualMachine :: Int -> ST s (VirtualMachine s)
initialVirtualMachine instructionPointerRegister = do
  registers <- MArr.newArray (minRegister, maxRegister) 0
  instructionPointer <- newSTRef 0
  isRunning <- newSTRef True
  snapshots <- newSTRef []
  return $ VM
    (maxRegister - minRegister + 1)
    registers
    instructionPointer
    instructionPointerRegister
    isRunning
    snapshots


runProgram :: VirtualMachine s -> String -> ST s (VirtualMachine s)
runProgram vm input = 
  flip runReaderT vm $ do
  let program = readProgram input
  run program
  ask
  

runAll :: Int -> String -> String
runAll reg0Value input =
  (unlines . reverse $ snapshots) ++ "\n" ++
  show register0
  where
    ipReg = readInstructionPointerRegister input
    (register0, snapshots) = 
      runST $ do
      vm <- initialVirtualMachine ipReg
      MArr.writeArray (_getRegisters vm) 0 reg0Value
      runProgram vm input
      reg0 <- MArr.readArray (_getRegisters vm) 0
      snaps <- readSTRef $ _getSnapshots vm
      return (reg0, snaps)

processPart1 :: String -> String
processPart1 input =
  runAll 0 input



root :: Int -> Int
root n =
  go 1 n
  where
    go l u
      | l + 1 == u = l
      | otherwise =
        let m = (l + u) `div` 2 in
        if m * m > n then go l m
        else go m u

primes :: Int -> [ Int ]
primes n =
  reverse . foldr consIfPrime [] $ reverse [2..n]

consIfPrime k primes =
  if all (\p -> (k `rem` p) /= 0) primes then k:primes
  else primes
      

primeDivisors :: Int -> [ Int ]
primeDivisors n =
  if bigPrime == 1 then smallPrimes
  else bigPrime:smallPrimes
  where
    bigPrime = foldr removePrime n smallPrimes
    smallPrimes = [ d | d <- primes (root n), n `rem` d == 0]
    removePrime p n
      | n `rem` p == 0 = removePrime p (n `div` p)
      | otherwise = n

sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (head:tail) = concat . map (\l -> [l, head:l]) $ sublists tail


divisors :: Int -> [ Int ]
divisors =
  map product . sublists . primeDivisors


processPart2 :: String -> String
processPart2 input =
  show $ divisors 10551374
      
