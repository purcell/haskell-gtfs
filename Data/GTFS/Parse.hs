{-# LANGUAGE
    ScopedTypeVariables
  , ViewPatterns
  , PatternGuards
  , OverloadedStrings #-}

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

import Data.Csv
import qualified Data.HashMap.Lazy as HM
import Safe (readMay)
import Data.List.Split
import Control.Applicative
import System.Directory
import System.IO.Unsafe ( unsafeInterleaveIO )
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.ByteString (ByteString)
import qualified Data.Vector as V

import qualified System.FilePath as Path

enumParseField :: forall a. (Enum a, Bounded a) => ByteString -> Parser a
enumParseField xs = parseField xs >>= conv where
  mx = fromEnum (maxBound :: a)
  conv n
    | n > mx    = fail ("out of range: " ++ show n)
    | otherwise = pure (toEnum n)

instance FromField LocationType  where parseField = enumParseField
instance FromField RouteType     where parseField = enumParseField
instance FromField DirectionID   where parseField = enumParseField
instance FromField OnOffType     where parseField = enumParseField
instance FromField ServiceFlag   where parseField = enumParseField
instance FromField ExceptionType where parseField = enumParseField
instance FromField PaymentMethod where parseField = enumParseField
instance FromField TransferType  where parseField = enumParseField
instance FromField Timepoint where parseField = enumParseField
instance FromField WheelchairAccessibility where parseField = enumParseField
instance FromField BikesAllowed where parseField = enumParseField

instance FromField Date where
  parseField x = parseField x >>= f where
    f (splitAt 4 -> (ys, splitAt 2 -> (ms, ds)))
      | length ds == 2
      , Just [y,m,d] <- mapM readMay [ys,ms,ds]
      = pure $ Date y m d
    f d = fail $ "invalid date: " ++ d

instance FromField Time where
  parseField x = parseField x >>= f where
    f (mapM readMay . splitOn ":" -> Just [h,m,s])
      = pure $ Time h m s
    f s = fail $ "invalid time: " ++ s

(.:?) :: FromField a => NamedRecord -> ByteString -> Parser (Maybe a)
(.:?) m name = maybe (pure Nothing) parseField $ HM.lookup name m

-- See https://developers.google.com/transit/gtfs/reference/

instance FromNamedRecord Agency where
  parseNamedRecord m = Agency <$>
    m .:? "agency_id" <*>
    m .: "agency_name" <*>
    m .: "agency_url" <*>
    m .: "agency_timezone" <*>
    m .:? "agency_lang" <*>
    m .:? "agency_phone" <*>
    m .:? "agency_fare_url" <*>
    m .:? "agency_email"
 
instance FromNamedRecord Stop where
  parseNamedRecord m = Stop <$>
    m .: "stop_id" <*>
    m .:? "stop_code" <*>
    m .: "stop_name" <*>
    m .:? "stop_desc" <*>
    m .: "stop_lat" <*>
    m .: "stop_lon" <*>
    m .:? "zone_id" <*>
    m .:? "stop_url" <*>
    m .:? "location_type" <*>
    m .:? "parent_station" <*>
    m .:? "stop_timezone" <*>
    m .:? "wheelchair_boarding"

instance FromNamedRecord Route where
  parseNamedRecord m = Route <$>
    m .: "route_id" <*>
    m .:? "agency_id" <*>
    m .: "route_short_name" <*>
    m .: "route_long_name" <*>
    m .:? "route_desc" <*>
    m .: "route_type" <*>
    m .:? "route_url" <*>
    m .:? "route_color" <*>
    m .:? "route_text_color" <*>
    m .:? "route_sort_order"

instance FromNamedRecord Trip where
  parseNamedRecord m = Trip <$>
    m .: "route_id" <*>
    m .: "service_id" <*>
    m .: "trip_id" <*>
    m .:? "trip_headsign" <*>
    m .:? "trip_short_name" <*>
    m .:? "direction_id" <*>
    m .:? "block_id" <*>
    m .:? "shape_id" <*>
    m .:? "wheelchair_accessible" <*>
    m .:? "bikes_allowed"

instance FromNamedRecord StopTime where
  parseNamedRecord m = StopTime <$>
    m .: "trip_id" <*>
    m .: "arrival_time" <*>
    m .: "departure_time" <*>
    m .: "stop_id" <*>
    m .: "stop_sequence" <*>
    m .:? "stop_headsign" <*>
    m .:? "pickup_type" <*>
    m .:? "drop_off_type" <*>
    m .:? "shape_dist_travelled" <*>
    m .:? "timepoint"

instance FromNamedRecord Calendar where
  parseNamedRecord m = Calendar <$>
    m .: "service_id" <*>
    m .: "monday" <*>
    m .: "tuesday" <*>
    m .: "wednesday" <*>
    m .: "thursday" <*>
    m .: "friday" <*>
    m .: "saturday" <*>
    m .: "sunday" <*>
    m .: "start_date" <*>
    m .: "end_date"

instance FromNamedRecord CalendarDate where
  parseNamedRecord m = CalendarDate <$>
    m .: "service_id" <*>
    m .: "date" <*>
    m .: "exception_type"

instance FromNamedRecord FareAttribute where
  parseNamedRecord m = FareAttribute <$>
    m .: "fare_id" <*>
    m .: "price" <*>
    m .: "currency_type" <*>
    m .: "payment_method" <*>
    m .: "transfers" <*>
    m .:? "agency_id" <*>
    m .:? "transfer_duration"

instance FromNamedRecord FareRule where
  parseNamedRecord m = FareRule <$>
    m .: "fare_id" <*>
    m .:? "route_id" <*>
    m .:? "origin_id" <*>
    m .:? "destination_id" <*>
    m .:? "contains_id"

instance FromNamedRecord Shape where
  parseNamedRecord m = Shape <$>
    m .: "shape_id" <*>
    m .: "shape_pt_lat" <*>
    m .: "shape_pt_lon" <*>
    m .: "shape_pt_sequence" <*>
    m .:? "shape_dist_travelled"

instance FromNamedRecord Frequency where
  parseNamedRecord m = Frequency <$>
    m .: "trip_id" <*>
    m .: "start_time" <*>
    m .: "end_time" <*>
    m .: "headway_secs" <*>
    m .:? "exact_times"

instance FromNamedRecord Transfer where
  parseNamedRecord m = Transfer <$>
    m .: "from_stop_id" <*>
    m .: "to_stop_id" <*>
    m .: "transfer_type" <*>
    m .:? "min_transfer_time"

instance FromNamedRecord FeedInfo where
  parseNamedRecord m = FeedInfo <$>
    m .: "feed_publisher_name" <*>
    m .: "feed_publisher_url" <*>
    m .: "feed_lang" <*>
    m .:? "feed_start_date" <*>
    m .:? "feed_end_date" <*>
    m .:? "feed_version"

-- | Parse a single GTFS data file.
--
-- Since some files are optional, this produces an empty list
-- if the file does not exist.
parseFile :: (FromNamedRecord a) => FilePath -> IO [a]
parseFile p = do
    ex <- doesFileExist p
    if not ex then return [] else go
  where
    go = do
      x <- decodeByName <$> BSL8.readFile p
      case x of
        Left e -> error ("parse failure: " ++ show e)
        Right (_header, ys) -> return (V.toList ys)

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
       <*> f "feed_info.txt"
  where
    f :: (FromNamedRecord a) => String -> IO [a]
    f x = unsafeInterleaveIO . parseFile $ Path.combine d x
