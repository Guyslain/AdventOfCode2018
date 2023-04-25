{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Day21 (processPart1, processPart2) where


import Data.Bits
import Data.Int
import Data.List (maximumBy, sort)
import Data.Array.IArray (Array, (!))
import Data.Array.Unboxed (UArray)
import qualified Data.Array.IArray as Arr
import Data.Array.MArray (MArray)
import qualified Data.Array.MArray as MArr
import Data.Array.ST (STArray, STUArray)

import Control.Monad (forM)
import Control.Monad.ST (ST, runST)
import Control.Monad.Reader

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM condition actionYes actionNo = do
  isYes <- condition
  if isYes then actionYes else actionNo
  
whenM :: Monad m => m Bool -> m () -> m ()
whenM condition action = do
  res <- condition
  if res then action else return ()

maxInt :: Int64
maxInt = 16777215

sixToEight :: (Int64, Int64, Int64) -> (Int64, Int64, Int64)
sixToEight (count, r3,r4) =
  ( count + 2
  , r4 .|. 65536
  , 10283511
  )

eightToTwelve :: (Int64, Int64, Int64) -> (Int64, Int64, Int64)
eightToTwelve (count,r3,r4) =
  ( count + 5
  , r3
  , e
  )
  where
    a = r3 .&. 255
    b = r4 + a
    c = b .&. maxInt
    d = c * 65899
    e = d .&. maxInt

twelveToSix :: (Int64, Int64, Int64) -> (Int64, Int64, Int64)
twelveToSix (count, r3, r4) = (count + 6, r3, r4)

backToEight :: (Int64, Int64, Int64) -> (Int64, Int64, Int64)
backToEight (count, r3,r4) =
  ( count + 4 + (r3 `div` 256) * 7
  , r3 `div` 256
  , r4
  )

getCount :: (Int64, Int64, Int64) -> Int64
getCount (count,_,_) = count
getR3 :: (Int64, Int64, Int64) -> Int64
getR3 (_,r3,_) = r3
getR4 :: (Int64, Int64, Int64) -> Int64
getR4 (_,_,r4) = r4


sixToExitTest :: (Int64, Int64, Int64) -> (Int64, Int64, Int64)
sixToExitTest =
  twelveToSix . head . dropWhile ((>= 256) . getR3) . future


future :: (Int64, Int64, Int64) -> [ (Int64, Int64, Int64) ]
future start =
  iterate (eightToTwelve . backToEight) (eightToTwelve . sixToEight $ start)


-- sixToSix :: Int64 -> (Int64, Int64)
-- sixToSix r4 = 

arcCosts :: UArray Int Int64
arcCosts =
  Arr.array (0, fromIntegral maxInt)
    [ (fromIntegral i, f i) | i <- [0..maxInt] ]
  where
    f init = 
     let (count,r3,r4) = sixToExitTest (0,0,init) in
     count

arcs :: UArray Int Int
arcs =
  Arr.array (0, fromIntegral maxInt)
   [ (fromIntegral i, f i) | i <- [0..maxInt] ]
  where
    f init =
      let (_,_,r4) = sixToExitTest (0,0,init) in
      fromIntegral r4


-- Longest path computation (not what we need)
-- data Walk
--   = Cycle Int64 Int
--   | Path Int64 Int
--   | Pending Int64
--   | Unknown

-- exploreNext :: MArray a Walk m =>
--                a Int Walk -> Int -> Int64 -> (Int64,Int) -> m (Int64, Int64, Int)
-- exploreNext array here cumulCost (arcCost,next) =
--   if next == here then do
--     MArr.writeArray array here (Cycle 0 here)
--     return (cumulCost + 1, 0, here)
--   else do
--     currentNextStatus <- MArr.readArray array next
--     case currentNextStatus of
--       Path cost end -> do
--         return (cumulCost + 1, arcCost + cost, end)
--       Cycle cost end ->
--         return (cumulCost + 1, arcCost + cost, end)
--       Pending pendingCost -> do
--         let cycleLength = cumulCost + arcCost - pendingCost
--         MArr.writeArray array next (Cycle (cycleLength - arcCost) here)
--         return (pendingCost, 0, here)
--       Unknown -> do
--         MArr.writeArray array here (Pending cumulCost)
--         (pendingCost,costAfterNext,end) <-
--           exploreNext array next (cumulCost + arcCost) (arcs!next)
--         if end == next then do
--           MArr.writeArray array next (Cycle costAfterNext end)
--           return (pendingCost, costAfterNext + arcCost, end)
--         else if pendingCost > cumulCost + arcCost then do
--           MArr.writeArray array next (Path costAfterNext end)
--           return (pendingCost, costAfterNext + arcCost, end)
--         else do
--           let endCost = fst $ arcs!end
--           let cycleCost = costAfterNext + cumulCost - pendingCost + endCost
--           MArr.writeArray array next (Cycle cycleCost here)
--           return (pendingCost, costAfterNext + arcCost, end)


-- exploreVertex :: MArray a Walk m =>
--                  a Int Walk -> Int -> m (Int,Int64)
-- exploreVertex array here = do
--   status <- MArr.readArray array here
--   case status of
--     Cycle cost end -> return (here, cost)
--     Path cost end -> return (here, cost)
--     Unknown -> do
--       MArr.writeArray array here (Pending 0)
--       (pendingCost, costAfterHere, end) <- exploreNext array here 0 (arcs!here)
--       if pendingCost == 0 then do
--         MArr.writeArray array here (Cycle costAfterHere end)
--         return (here, costAfterHere)
--       else do
--         MArr.writeArray array here (Path costAfterHere end)
--         return (here, costAfterHere)


-- longest non-cycle path computation

type Length = Int64
type Vertex = Int
data Status = Unknown | Pending Length | Known (Length, Vertex)


class Monad m => LongestPathContextM m where
  getMaxVertex :: m Vertex
  setLength :: Vertex -> Length -> Vertex -> m ()
  getLength :: Vertex -> m (Maybe (Length, Vertex))
  setPending :: Vertex -> m ()
  isPending :: Vertex -> m Bool
  getNext :: Vertex -> m Vertex
  getNextCost :: Vertex -> m Length


computePathExtremity :: LongestPathContextM m =>
                     Length -> Vertex -> m (Length, Vertex)
computePathExtremity lengthToHere here = do
  status <- getLength here
  case status of
    Just (length, end) -> do
      return (lengthToHere + length, end)
    Nothing -> do
      setPending here
      next <- getNext here
      nextCost <- getNextCost here
      ifM (isPending next)
        (return (lengthToHere, here))
        (computePathExtremity (lengthToHere + nextCost) next)

setStatus :: LongestPathContextM m =>
             (Length, Vertex) -> Vertex -> m ()
setStatus (lengthToEnd, end) here =
  whenM (isPending here) $ do
  endNext <- getNext end
  if endNext == here then do
    endCost <- getNextCost end
    setCyclicStatus (lengthToEnd + endCost) here end here
  else do
    setLength here lengthToEnd end
    next <- getNext here
    cost <- getNextCost here
    setStatus (lengthToEnd - cost, end) next
    
setCyclicStatus cycleLength start pred here = do
  predCost <- getNextCost pred
  setLength here (cycleLength - predCost) pred
  next <- getNext here
  if (next == start) then
    return ()
  else
    setCyclicStatus cycleLength start here next


determineLength :: LongestPathContextM m =>
             Vertex -> m Length
determineLength here = do
  status <- getLength here
  case status of
    Nothing -> do
      (length,end) <- computePathExtremity 0 here
      setStatus (length,end) here
      return length
    Just (length,end) -> return length
             


findLongestPath :: LongestPathContextM m => m Length
findLongestPath = do
  maxVertex <- getMaxVertex
  lengths <- forM [0..maxVertex] determineLength
  return $ maximum lengths
  

    

data Context s =
  Context
  { _getPendings :: STUArray s Vertex Bool
  , _getEnds :: STUArray s Vertex Vertex
  , _getLengths :: STUArray s Vertex Length
  , _maxVertex :: Vertex
  }

initContext :: Vertex -> ST s (Context s)
initContext maxVertex = do
  pendings <- MArr.newArray (0,maxVertex) False
  lengths <- MArr.newArray (0,maxVertex) (-1)
  ends <- MArr.newArray (0,maxVertex) (-1)
  return $ Context
   { _getPendings = pendings
   , _getEnds = ends
   , _getLengths = lengths
   , _maxVertex = maxVertex
   }
  
  
instance  LongestPathContextM (ReaderT (Context s) (ST s)) where
  getMaxVertex = asks _maxVertex
  getLength vertex = do
    lengths <- asks _getLengths
    ends <- asks _getEnds
    l <- lift $ MArr.readArray lengths vertex
    e <- lift $ MArr.readArray ends vertex
    return $ if (l == -1) then Nothing else Just (l,e)
  setLength vertex length end = do
    lengths <- asks _getLengths
    ends <- asks _getEnds
    pendings <- asks _getPendings
    lift $ MArr.writeArray lengths vertex length
    lift $ MArr.writeArray ends vertex end
    lift $ MArr.writeArray pendings vertex False
  setPending vertex = do
    pendings <- asks _getPendings
    lift $ MArr.writeArray pendings vertex True
  isPending vertex = do
    pendings <- _getPendings <$> ask
    lift $ MArr.readArray pendings vertex
  getNext vertex =
    return $ arcs!vertex
  getNextCost vertex =
    return $ arcCosts!vertex
 

longestPathFrom0 :: (Length, Vertex, Vertex)
longestPathFrom0 =
  runST $ do
  context <- initContext $ fromIntegral maxInt
  len <- runReaderT (determineLength 0) context
  end <- MArr.readArray (_getEnds context) 0
  return (len, 0, end)
    

longestStarts :: [ (Length, Vertex, Vertex) ]
longestStarts =
  runST $ do
  context <- initContext $ fromIntegral maxInt
  maxLen <- runReaderT findLongestPath context
  maxStarts <- filterM (\v -> (== maxLen) <$> MArr.readArray (_getLengths context) v) [0..fromIntegral maxInt]
  forM maxStarts
    (\v -> do
        len <- MArr.readArray (_getLengths context) v
        end <- MArr.readArray (_getEnds context) v
        return (len, v, end)
    )
  

compareBy :: Ord b => (a -> b) -> (a -> a -> Ordering)
compareBy f a b = compare (f a) (f b)

lengthOrdering :: (Vertex, Maybe Length) -> (Vertex, Maybe Length) -> Ordering
lengthOrdering = compareBy snd 



processPart1 :: String -> String
processPart1 input =
  show
  . sixToExitTest
  $ (0,0,15675199)


-- 15609663


processPart2 :: String -> String
processPart2 input =
  show $ longestPathFrom0

processPart3 :: String -> String
processPart3 input =
--  unlines . map show . take 1000 . future $ (0,0,15675199)
--  show . sum $ Arr.elems arcCosts
  unlines
  . map (\(len,start,end) -> show len ++ " " ++ show start ++ " " ++ show end)
  $ longestStarts
