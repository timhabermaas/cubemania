{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import GHC.Generics
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.=), object)
import Servant (FromHttpApiData(..))
import Data.Text (Text)
import Data.Char (chr)
import Data.Time.LocalTime (LocalTime, localTimeToUTC, utc)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Clock (UTCTime)
import Data.Word (Word8)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import qualified Data.ByteString as BS
import Control.Monad (mzero)
import Database.PostgreSQL.Simple

type DurationInMs = Int

data Configuration = Configuration
  { getPool :: Connection
  }

newtype PuzzleId = PuzzleId Int deriving (Generic)
instance FromHttpApiData PuzzleId where
  parseUrlPiece t = PuzzleId <$> parseInt t
    where
      parseInt :: Text -> Either Text Int
      parseInt = parseUrlPiece
instance ToJSON PuzzleId
instance FromField PuzzleId where
    fromField f s = PuzzleId <$> fromField f s

newtype Limit = Limit Int deriving (Generic)
instance FromField Limit where
    fromField f s = Limit <$> fromField f s
instance FromHttpApiData Limit where
  parseUrlPiece t = Limit <$> parseInt t
    where
      parseInt :: Text -> Either Text Int
      parseInt = parseUrlPiece

newtype SingleId = SingleId Int deriving (Generic, Show, Eq)
instance FromField SingleId where
    fromField f s = SingleId <$> fromField f s
instance FromHttpApiData SingleId where
  parseUrlPiece t = SingleId <$> parseInt t
    where
      parseInt :: Text -> Either Text Int
      parseInt = parseUrlPiece

newtype UserId = UserId Int deriving (Generic, Show, Eq)
instance FromField UserId where
    fromField f s = UserId <$> fromField f s
instance FromRow UserId
instance ToJSON UserId
instance FromHttpApiData UserId where
  parseUrlPiece t = UserId <$> parseInt t
    where
      parseInt :: Text -> Either Text Int
      parseInt = parseUrlPiece

instance ToJSON SingleId

data Penalty = Plus2 | Dnf deriving (Generic)
instance ToJSON Penalty where
    toJSON Plus2 = String "plus2"
    toJSON Dnf   = String "dnf"

word8ToString :: [Word8] -> String
word8ToString = Prelude.map (chr . fromIntegral)

instance FromField Penalty where
    fromField field dat =
        case BS.unpack <$> dat of
          Nothing -> returnError UnexpectedNull field ""
          Just v -> case word8ToString v of
            "plus2" -> return Plus2
            "dnf"   -> return Dnf
            x       -> returnError ConversionFailed field x

data Single = Single
    { singleId :: SingleId
    , singleTime :: DurationInMs
    , singleComment :: Maybe String
    , singleScramble :: String
    , singlePenalty :: Maybe Penalty
    , singleCreatedAt :: UTCTime
    , singleUserId :: UserId
    } deriving (Generic)

instance ToJSON Single where
    toJSON (Single {..}) = object
      [ "id" .= singleId
      , "time" .= singleTime
      , "comment" .= singleComment
      , "scramble" .= singleScramble
      , "penalty" .= singlePenalty
      , "created_at" .= singleCreatedAt
      ]



instance FromRow Single where
    fromRow = Single <$> field <*> field <*> field <*> field <*> field <*> ((localTimeToUTC utc) <$> field) <*> field

-- TODO: Add prefix and use custom FromJSON instance.
data SubmittedSingle = SubmittedSingle
    { scramble :: String
    , time :: DurationInMs
    } deriving (Generic)

instance FromJSON SubmittedSingle

data RecordSingle = RecordSingle
    { recordSingleId :: SingleId
    , recordSingleTime :: DurationInMs
    , recordSingleScramble :: String
    }

instance FromRow RecordSingle where
    fromRow = RecordSingle <$> field <*> field <*> field

instance ToJSON RecordSingle where
    toJSON (RecordSingle{..}) = object
      [ "id" .= recordSingleId
      , "time" .= recordSingleTime
      , "scramble" .= recordSingleScramble
      ]

data SimpleUser = SimpleUser
    { simpleUserId :: UserId
    , simpleUserSlug :: String
    , simpleUserName :: String
    , simpleUserSinglesCount :: Int
    }

instance ToJSON SimpleUser where
    toJSON (SimpleUser {..}) = object
      [ "id" .= simpleUserId
      , "slug" .= simpleUserSlug
      , "name" .= simpleUserName
      , "singles_count" .= simpleUserSinglesCount
      ]

instance FromRow SimpleUser where
    fromRow = SimpleUser <$> field <*> field <*> field <*> field

-- TODO get rid of Type prefix
data RecordType = TypeSingle | TypeAverage5 | TypeAverage12

instance FromField RecordType where
    fromField f s = do
        a <- fromField f s
        case (a :: Int) of
          1  -> return TypeSingle
          5  -> return TypeAverage5
          12 -> return TypeAverage12
          _  -> mzero

instance ToJSON RecordType where
    toJSON TypeSingle = String "Single"
    toJSON TypeAverage5 = String "Average of 5"
    toJSON TypeAverage12 = String "Average of 12"

data Record = Record
    { recordId :: Int
    , recordTime :: DurationInMs
    , recordComment :: String
    , recordPuzzleId :: PuzzleId
    , recordType :: RecordType
    , recordSingles :: [RecordSingle]
    } deriving (Generic)

instance FromRow Record where
    fromRow = Record <$> field <*> field <*> field <*> field <*> field <*> pure []

instance ToJSON Record where
    toJSON Record{..} = object
        [ "id" .= recordId
        , "time" .= recordTime
        , "set_at" .= (0 :: Int)
        , "comment" .=  recordComment
        , "puzzle_id" .= recordPuzzleId
        , "type_full_name" .= recordType
        , "singles" .= recordSingles
        ]

data ChartData = ChartData
    { chartTime :: Double
    , chartComment :: Maybe String
    , chartCreatedAt :: UTCTime
    }

instance FromRow ChartData where
    fromRow = ChartData <$> (fromRational <$> field) <*> field <*> (localTimeToUTC utc <$> field)
instance ToJSON ChartData where
    toJSON ChartData{..} = object
        [ "time" .= chartTime
        , "created_at" .= chartCreatedAt
        , "created_at_timestamp" .= utcTimeToEpoch chartCreatedAt
        ]
      where
        utcTimeToEpoch :: UTCTime -> Int
        utcTimeToEpoch time = read $ formatTime defaultTimeLocale "%s" time

data ChartGroup = Month | Week | Day

newtype LocalTimeWithFromRow = LocalTimeWithFromRow LocalTime deriving (Generic)
instance FromRow LocalTimeWithFromRow

localTimeToUTCTime :: LocalTimeWithFromRow -> UTCTime
localTimeToUTCTime (LocalTimeWithFromRow t) = localTimeToUTC utc t
