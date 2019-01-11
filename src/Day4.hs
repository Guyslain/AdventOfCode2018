{-# LANGUAGE OverloadedStrings #-}

module Day4 (processPart1, processPart2) where

import Text.Trifecta hiding (count)
import Text.Parser.Token
import qualified Data.Text as T

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.List (sortBy, groupBy, foldl, maximumBy)
import Data.Array.IArray
import GHC.Exts (sortWith)

import Control.Applicative

toDiffTime :: Int -> Int -> DiffTime
toDiffTime hour minute =
  timeOfDayToTime
  $ TimeOfDay hour minute 0

data EventInfo =
    Shift Int
  | FallsAsleep
  | WakesUp
  deriving Show

data Event =
  Event { time :: UTCTime, info :: EventInfo }
  deriving Show

data Night =
  Night { guard :: Int, events :: [Event] }
  deriving Show

data GuardStats =
  Stats
    { guardId :: Int,
      minutesSlept :: Int,
      timetable :: Array Int Int 
    }

dummyGuard :: GuardStats
dummyGuard = Stats (-1) 0 (array (0, 59) [ (i,0) | i <- [0..59] ])

infoParser :: Parser EventInfo
infoParser =
  (do
      _ <- textSymbol "Guard #"
      guardId <- fmap fromInteger integer
      _ <- textSymbol "begins shift"
      return $ Shift guardId
  ) <|>
  (do
     _ <- textSymbol "falls asleep"
     return FallsAsleep
  ) <|>
  (do
     _ <- textSymbol "wakes up"
     return WakesUp
  )

eventParser :: Parser Event
eventParser = do
  _ <- symbolic '['
  year <- integer
  _ <- symbolic '-'
  month <- int 
  _ <- symbolic '-'
  day <- int
  hour <- int
  _ <- symbolic ':'
  minute <- int
  _ <- symbolic ']'
  info <- infoParser
  return $
    Event 
      (UTCTime (fromGregorian year month day) (toDiffTime hour minute))
      info
  where int = fmap fromInteger integer


parseEvent :: String -> Event
parseEvent line =
  case parseString eventParser mempty line of
    Success event -> event


partitionAt :: (a -> Bool) -> [a] -> [[a]]
partitionAt condition = go [] []
  where
    go groups group [] = reverse (reverse group : groups)
    go groups group (x:xs)
      | condition x && null group = go groups [x] xs
      | condition x = go (reverse group : groups) [x] xs
      | otherwise = go groups (x:group) xs



annotateByGuard events@(Event _ (Shift guard) : _) =
  Night guard events

  
        
-- groupEventsByGuard :: [Event] -> [[Night]]
groupEventsByGuard =
    groupBy sameGuard
  . sortWith guard
  . map annotateByGuard
  . partitionAt (isShift . info)
  . sortWith time
  where
    isShift (Shift _) = True
    isShift _ = False
    sameGuard night1 night2 = guard night1 == guard night2

minuteOfTime :: UTCTime -> Int
minuteOfTime =
  todMin
  . localTimeOfDay
  . utcToLocalTime utc


recordNightStats :: GuardStats -> Night -> GuardStats
recordNightStats guard =
  fst . foldl recordEvent (guard, 0) . events
    where
      recordEvent (guard, nextMinute) (Event time (Shift gid)) =
        (Stats gid (minutesSlept guard) (timetable guard), nextMinute)
      recordEvent (guard, nextMinute) (Event time FallsAsleep) =
        (guard, minuteOfTime time)
      recordEvent (Stats gid minSlept table, nextMinute) (Event time WakesUp) =
        (Stats
          gid
          (minSlept + minuteNow - nextMinute)
          (table // [(i, table ! i + 1) | i <- [nextMinute .. minuteNow - 1]]),
         minuteNow
        )
        where minuteNow = minuteOfTime time 

recordAllNights :: [ Night ] -> GuardStats
recordAllNights =
  foldl recordNightStats dummyGuard

compileGuardsStats :: [[Night]] -> [ GuardStats ]
compileGuardsStats = map recordAllNights


data Candidate =
  Candidate
  { guardStats :: GuardStats
  , minute :: Int
  , count :: Int
  }
    
    

maxSleptMinute :: GuardStats -> Candidate
maxSleptMinute stats@(Stats gid _ table) =
  Candidate stats index max
  where
    (index, max) =
      head
      . reverse
      . sortBy (\(i1,c1) (i2, c2) -> compare c1 c2)
      $ assocs table

strategy1 :: [GuardStats] -> Candidate
strategy1 =
  maxSleptMinute
  . maximumBy (\st1 st2 -> compare (minutesSlept st1) (minutesSlept st2))

strategy2 :: [GuardStats] -> Candidate
strategy2 =
  maximumBy (\c1 c2 -> compare (count c1) (count c2))
  . map maxSleptMinute


preProcess :: String -> [GuardStats]
preProcess =
    compileGuardsStats
  . groupEventsByGuard
  . map parseEvent
  . lines


postProcess :: Candidate -> String
postProcess =
    show
  . (\ cand -> (guardId . guardStats $ cand) * (minute cand))

processPart1 :: String -> String
processPart1 =
  postProcess
  . strategy1
  . preProcess


processPart2 :: String -> String
processPart2 = 
  postProcess
  . strategy2
  . preProcess
  
