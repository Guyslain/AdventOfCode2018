{-# LANGUAGE OverloadedStrings #-}

module Day4 (processPart1) where

import Text.Trifecta
import Text.Parser.Token
import qualified Data.Text as T

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime

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
  Event UTCTime EventInfo
  deriving Show


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

processPart1 :: String -> String
processPart1 =
  show
  . map parseEvent
  . lines
