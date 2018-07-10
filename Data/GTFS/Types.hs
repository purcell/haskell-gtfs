-- | Types in a GTFS feed.

module Data.GTFS.Types
  -- export everything
  where

import Network.URI (URI)

-- * Enums

data LocationType
  = LocStop
  | LocStation
  deriving (Show, Enum, Bounded, Eq, Ord)

data RouteType
  = Tram
  | Metro
  | Rail
  | Bus
  | Ferry
  | CableCar
  | Gondola
  | Funicular
  deriving (Show, Enum, Bounded, Eq, Ord)

data DirectionID
  = DirectionA
  | DirectionB
  deriving (Show, Enum, Bounded, Eq, Ord)

data OnOffType
  = RegularlyScheduled
  | NotAvailable
  | MustPhone
  | MustAskDriver
  deriving (Show, Enum, Bounded, Eq, Ord)

data ServiceFlag
  = NoService
  | HasService
  deriving (Show, Enum, Bounded, Eq, Ord)

data ExceptionType
  = NoException
  | ServiceAdded
  | ServiceRemoved
  deriving (Show, Enum, Bounded, Eq, Ord)

data PaymentMethod
  = PayOnBoard
  | PayBeforeBoarding
  deriving (Show, Enum, Bounded, Eq, Ord)

data TransferType
  = RecommendedTransfer
  | TimedTransfer
  | MinimumTransfer
  | NoTransfer
  deriving (Show, Enum, Bounded, Eq, Ord)

data WheelchairAccessibility
  = SomeWheelchairAccessibility
  | NoWheelchairAccessibility
  deriving (Show, Enum, Bounded, Eq, Ord)

data BikesAllowed
  = SomeBikesAllowed
  | NoBikesAllowed
  deriving (Show, Enum, Bounded, Eq, Ord)

data Timepoint
  = ApproximateTimepoint
  | ExactTimepoint
  deriving (Show, Enum, Bounded, Eq, Ord)

-- * Row types

data Agency = Agency
  { a_agency_id       :: Maybe AgencyID
  , a_agency_name     :: String
  , a_agency_url      :: URL
  , a_agency_timezone :: Timezone
  , a_agency_lang     :: Maybe Language
  , a_agency_phone    :: Maybe Phone
  , a_agency_fare_url :: Maybe URL
  , a_agency_email    :: Maybe Email
  } deriving (Show)

data Stop = Stop
  { s_stop_id        :: StopID
  , s_stop_code      :: Maybe String
  , s_stop_name      :: String
  , s_stop_desc      :: Maybe String
  , s_stop_lat       :: LatLon
  , s_stop_lon       :: LatLon
  , s_zone_id        :: Maybe ZoneID
  , s_stop_url       :: Maybe URL
  , s_location_type  :: Maybe LocationType
  , s_parent_station :: Maybe StopID
  , s_stop_timezone  :: Maybe Timezone
  , s_wheelchair_boarding :: Maybe WheelchairAccessibility
  } deriving (Show)

data Route = Route
  { r_route_id         :: RouteID
  , r_agency_id        :: Maybe AgencyID
  , r_route_short_name :: String
  , r_route_long_name  :: String
  , r_route_desc       :: Maybe String
  , r_route_type       :: RouteType
  , r_route_url        :: Maybe URL
  , r_route_color      :: Maybe Color
  , r_route_text_color :: Maybe Color
  , r_route_sort_order :: Maybe Integer
  } deriving (Show)

data Trip = Trip
  { t_route_id        :: RouteID
  , t_service_id      :: ServiceID
  , t_trip_id         :: TripID
  , t_trip_headsign   :: Maybe String
  , t_trip_short_name :: Maybe String
  , t_direction_id    :: Maybe DirectionID
  , t_block_id        :: Maybe BlockID
  , t_shape_id        :: Maybe ShapeID
  , t_wheelchair_accessible :: Maybe WheelchairAccessibility
  , t_bikes_allowed :: Maybe BikesAllowed
  } deriving (Show)

data StopTime = StopTime
  { st_trip_id             :: TripID
  , st_arrival_time        :: Time
  , st_departure_time      :: Time
  , st_stop_id             :: StopID
  , st_stop_sequence       :: Sequence
  , st_stop_headsign       :: Maybe String
  , st_pickup_type         :: Maybe OnOffType
  , st_drop_off_type       :: Maybe OnOffType
  , st_shape_dist_traveled :: Maybe Distance
  , st_timepoint :: Maybe Timepoint
  } deriving (Show)

data Calendar = Calendar
  { c_service_id :: ServiceID
  , c_monday     :: ServiceFlag 
  , c_tuesday    :: ServiceFlag 
  , c_wednesday  :: ServiceFlag 
  , c_thursday   :: ServiceFlag 
  , c_friday     :: ServiceFlag 
  , c_saturday   :: ServiceFlag 
  , c_sunday     :: ServiceFlag 
  , c_start_date :: Date
  , c_end_date   :: Date
  } deriving (Show)

data CalendarDate = CalendarDate
  { cd_service_id     :: ServiceID
  , cd_date           :: Date
  , cd_exception_type :: ExceptionType
  } deriving (Show)

data FareAttribute = FareAttribute
  { fa_fare_id           :: FareID
  , fa_price             :: Price
  , fa_currency_type     :: Currency
  , fa_payment_method    :: PaymentMethod
  , fa_transfers         :: TransferLimit
  , fa_agency_id         :: Maybe AgencyID
  , fa_transfer_duration :: Maybe Seconds
  } deriving (Show)

data FareRule = FareRule
  { fr_fare_id        :: FareID
  , fr_route_id       :: Maybe RouteID 
  , fr_origin_id      :: Maybe ZoneID
  , fr_destination_id :: Maybe ZoneID
  , fr_contains_id    :: Maybe ZoneID
  } deriving (Show)

data Shape = Shape
  { sh_shape_id            :: ShapeID
  , sh_shape_pt_lat        :: LatLon
  , sh_shape_pt_lon        :: LatLon
  , sh_shape_pt_sequence   :: Sequence
  , sh_shape_dist_traveled :: Maybe Distance
  } deriving (Show)

data Frequency = Frequency
  { fq_trip_id      :: TripID
  , fq_start_time   :: Time
  , fq_end_time     :: Time
  , fq_headway_secs :: Seconds
  , fq_exact_times  :: Maybe Timepoint
  } deriving (Show)

data Transfer = Transfer
  { x_from_stop_id      :: StopID
  , x_to_stop_id        :: StopID
  , x_transfer_type     :: TransferType
  , x_min_transfer_time :: Maybe Seconds
  } deriving (Show)

data FeedInfo = FeedInfo
  { fi_feed_publisher_name :: String
  , fi_feed_publisher_url  :: URL
  , fi_feed_lang           :: Language
  , fi_feed_start_date     :: Maybe Date
  , fi_feed_end_date       :: Maybe Date
  , fi_feed_version        :: Maybe String
  } deriving (Show)


-- * The feed itself

data Feed = Feed
  { f_agency          :: [Agency]
  , f_stops           :: [Stop]
  , f_routes          :: [Route]
  , f_trips           :: [Trip]
  , f_stop_times      :: [StopTime]
  , f_calendar        :: [Calendar]
  , f_calendar_dates  :: [CalendarDate]
  , f_fare_attributes :: [FareAttribute]
  , f_fare_rules      :: [FareRule]
  , f_shapes          :: [Shape]
  , f_frequencies     :: [Frequency]
  , f_transfers       :: [Transfer]
  , f_feed_infos      :: [FeedInfo]
  } deriving (Show)


-- * Date and time

-- | Year, month, day.
data Date = Date Int Int Int
  deriving (Show)

-- | Hour, minute, second.
--
-- Hours over 23 are legal, representing the next day
-- relative to the start of a trip.
data Time = Time Int Int Int
  deriving (Show)


-- * Type synonyms

type AgencyID  = String
type BlockID   = String
type FareID    = String
type RouteID   = String
type ServiceID = String
type ShapeID   = String
type StopID    = String
type TripID    = String
type ZoneID    = String

type URL      = URI
type Email    = String
type Phone    = String
type Timezone = String
type Language = String
type Currency = String
type Color    = String

type Sequence = Int
type Seconds  = Int
type Distance = Double -- Rational?
type Price    = Double -- Rational?
type LatLon   = Double -- Rational?

type TransferLimit = Maybe Int
