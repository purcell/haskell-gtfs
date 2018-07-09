{-# LANGUAGE
    ScopedTypeVariables
  , ViewPatterns
  , PatternGuards
  , TemplateHaskell #-}

{-# OPTIONS_GHC
  -fno-warn-orphans #-}

-- | Parsing GTFS files.
--
-- Besides these functions, this module provides many orphan
-- instances of @'Field'@ and @'ParseRow'@.

module Data.GTFS.Parse
  ( parseFile
  , parseFeed
  ) where

import Data.GTFS.Types

import Text.RowRecord
import Text.RowRecord.TH

import Data.List.Split
import Control.Applicative
import System.Directory
import System.IO.Unsafe ( unsafeInterleaveIO )

import qualified Text.CSV        as CSV
import qualified System.FilePath as Path

enumDecode :: forall a. (Enum a, Bounded a) => Maybe String -> Result a
enumDecode xs = decode xs >>= conv where
  mx = fromEnum (maxBound :: a)
  conv n
    | n > mx    = Failure $ NoParse "" ("out of range: " ++ show n)
    | otherwise = Success $ toEnum n

instance Field LocationType  where decode = enumDecode
instance Field RouteType     where decode = enumDecode
instance Field DirectionID   where decode = enumDecode
instance Field OnOffType     where decode = enumDecode
instance Field ServiceFlag   where decode = enumDecode
instance Field ExceptionType where decode = enumDecode
instance Field PaymentMethod where decode = enumDecode
instance Field TransferType  where decode = enumDecode

instance Field Date where
  decode = require f where
    f (splitAt 4 -> (ys, splitAt 2 -> (ms, ds)))
      | length ds == 2
      , Just [y,m,d] <- mapM safeRead [ys,ms,ds]
      = Just $ Date y m d
    f _ = Nothing

instance Field Time where
  decode = require f where
    f (mapM safeRead . splitOn ":" -> Just [h,m,s])
      = Just $ Time h m s
    f _ = Nothing

$(rowRecords [
    ''Agency
  , ''Stop
  , ''Route
  , ''Trip
  , ''StopTime
  , ''Calendar
  , ''CalendarDate
  , ''FareAttribute
  , ''FareRule
  , ''Shape
  , ''Frequency
  , ''Transfer ])

-- drop some bad rows from a CSV file
cleanup :: [[String]] -> [[String]]
cleanup = filter ok where
  ok []   = False
  ok [""] = False
  ok _    = True

getCSV :: FilePath -> IO [[String]]
getCSV p = do
  x <- CSV.parseCSVFromFile p
  case x of
    Left  e -> error ("CSV parse failed on " ++ p ++ ": " ++ show e)
    Right v -> return . cleanup $ v

-- | Parse a single GTFS data file.
--
-- Since some files are optional, this produces an empty list
-- if the file does not exist.
parseFile :: (ParseRow a) => FilePath -> IO [a]
parseFile p = do
    ex <- doesFileExist p
    if not ex then return [] else go
  where
    go = do
      x <- getCSV p
      case fromStrings x >>= parseTable of
        Failure e -> error ("field parse failure: " ++ show e)
        Success y -> return y

-- | Parse an entire feed directory.
--
-- Each individual file is read and parsed only when its field in @'Feed'@
-- is forced.  The usual caveats of lazy I\/O apply.  Parsing within a file
-- is not lazy.
-- 
-- Alternatives to this function include @'parseFile'@ and @'parseRow'@.
parseFeed :: FilePath -> IO Feed
parseFeed d =
  Feed <$> f "agency.txt"         <*> f "stops.txt"           <*> f "routes.txt"
       <*> f "trips.txt"          <*> f "stop_times.txt"      <*> f "calendar.txt"
       <*> f "calendar_dates.txt" <*> f "fare_attributes.txt" <*> f "fare_rules.txt"
       <*> f "shapes.txt"         <*> f "frequencies.txt"     <*> f "transfers.txt"
  where
    f :: (ParseRow a) => String -> IO [a]
    f x = unsafeInterleaveIO . parseFile $ Path.combine d x
