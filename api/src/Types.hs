{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types where

import Prelude hiding (id)
import GHC.Generics
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.=), object, (.:), (.:?))
import Servant (FromHttpApiData(..))
import Web.HttpApiData (ToHttpApiData, toQueryParam)
import Data.Text (Text, pack)
import Data.Char (chr)
import Data.Time.LocalTime (LocalTime, localTimeToUTC, utc)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Clock (UTCTime)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import qualified Data.ByteString as BS
import Control.Monad (mzero)
import Text.Blaze.Html (ToMarkup(..))

type DurationInMs = Int

newtype PageNumber = PageNumber Int
fromPageNumber :: Num a => PageNumber -> a
fromPageNumber (PageNumber x) = fromIntegral x
nextPage :: PageNumber -> PageNumber
nextPage (PageNumber x) = PageNumber (succ x)

instance ToHttpApiData PageNumber where
    toQueryParam (PageNumber i) = toQueryParam i
instance FromHttpApiData PageNumber where
    parseUrlPiece t = PageNumber <$> parseUrlPiece t

newtype PuzzleId = PuzzleId Int deriving (Generic, Eq, Ord)
instance FromHttpApiData PuzzleId where
    parseUrlPiece t = PuzzleId <$> parseUrlPiece t
instance ToJSON PuzzleId
instance FromField PuzzleId where
    fromField f s = PuzzleId <$> fromField f s
instance ToField PuzzleId where
    toField (PuzzleId id) = toField id
newtype PuzzleSlug = PuzzleSlug Text deriving (Show, Eq, Generic)
instance FromField PuzzleSlug where
    fromField f s = PuzzleSlug <$> fromField f s
instance ToField PuzzleSlug where
    toField (PuzzleSlug slug) = toField slug
instance ToJSON PuzzleSlug
instance FromHttpApiData PuzzleSlug where
    parseUrlPiece t = PuzzleSlug <$> parseUrlPiece t

newtype KindId = KindId Int deriving (Eq, Generic)
instance FromField KindId where
    fromField f s = KindId <$> fromField f s
instance ToField KindId where
    toField (KindId id) = toField id
instance ToJSON KindId

newtype Limit = Limit Int deriving (Generic)
instance FromField Limit where
    fromField f s = Limit <$> fromField f s
instance FromHttpApiData Limit where
    parseUrlPiece t = Limit <$> parseUrlPiece t

newtype AnnouncementId = AnnouncementId Int deriving (Eq)
instance Show AnnouncementId where
    show (AnnouncementId x) = show x
instance FromField AnnouncementId where
    fromField f s = AnnouncementId <$> fromField f s
instance ToField AnnouncementId where
    toField (AnnouncementId id) = toField id
instance FromHttpApiData AnnouncementId where
    parseUrlPiece t = AnnouncementId <$> parseUrlPiece t

newtype CommentId = CommentId Int deriving Show
instance FromField CommentId where
    fromField f s = CommentId <$> fromField f s

newtype SingleId = SingleId Int deriving (Generic, Show, Eq)
instance FromField SingleId where
    fromField f s = SingleId <$> fromField f s
instance FromHttpApiData SingleId where
  parseUrlPiece s = SingleId <$> parseUrlPiece s
instance ToJSON SingleId

newtype UserId = UserId Int deriving (Generic, Show, Eq, Ord)
instance FromField UserId where
    fromField f s = UserId <$> fromField f s
instance FromRow UserId
instance ToJSON UserId
instance ToField UserId where
    toField (UserId id) = toField id
instance FromHttpApiData UserId where
    parseUrlPiece u = UserId <$> parseUrlPiece u
newtype UserSlug = UserSlug Text deriving (Show, Generic)
instance ToField UserSlug where
    toField (UserSlug slug) = toField slug
instance FromField UserSlug where
    fromField f s = UserSlug <$> fromField f s
instance FromHttpApiData UserSlug where
    parseUrlPiece u = UserSlug <$> parseUrlPiece u
instance ToJSON UserSlug

fromSlug :: UserSlug -> Text
fromSlug (UserSlug s) = s


data Penalty = Plus2 | Dnf deriving (Show, Eq)
instance ToJSON Penalty where
    toJSON Plus2 = String "plus2"
    toJSON Dnf   = String "dnf"
instance ToField Penalty where
    toField Plus2 = Escape $ "plus2"
    toField Dnf = Escape $ "dnf"
instance FromJSON Penalty where
    parseJSON (String "dnf") = pure Dnf
    parseJSON (String "plus2") = pure Plus2
    parseJSON _ = fail "foo"

word8ToString :: [Word8] -> String
word8ToString = Prelude.map (chr . fromIntegral)

instance FromField Penalty where
    fromField f dat =
        case BS.unpack <$> dat of
          Nothing -> returnError UnexpectedNull f ""
          Just v -> case word8ToString v of
            "plus2" -> return Plus2
            "dnf"   -> return Dnf
            x       -> returnError ConversionFailed f x

data Announcement = Announcement
    { announcementId :: AnnouncementId
    , announcementTitle :: Text
    , announcementContent :: Text
    , announcementUserId :: UserId
    , announcementCreatedAt :: UTCTime
    }

instance FromRow Announcement where
    fromRow = Announcement <$> field <*> field <*> field <*> field <*> ((localTimeToUTC utc) <$> field)

data Comment = Comment
    { commentId :: CommentId
    , commentContent :: Text
    , commentAuthorId :: Maybe UserId
    , commentCreatedAt :: UTCTime
    }

instance FromRow Comment where
    fromRow = Comment <$> field <*> field <*> field <*> ((localTimeToUTC utc) <$> field)

data Single = Single
    { singleId :: !SingleId
    , singleTime :: !DurationInMs
    , singleComment :: !(Maybe Text)
    , singleScramble :: !Text
    , singlePenalty :: !(Maybe Penalty)
    , singleCreatedAt :: !UTCTime
    , singleUserId :: !UserId
    } deriving (Show, Eq)

isDnf :: Single -> Bool
isDnf (Single _ _ _ _ (Just Dnf) _ _) = True
isDnf _ = False

instance Ord Single where
    compare (Single _ _ _ _ (Just Dnf) _ _) (Single _ _ _ _ (Just Dnf) _ _) = EQ
    compare (Single _ _ _ _ (Just Dnf) _ _) (Single _ _ _ _ _ _ _) = GT
    compare (Single _ _ _ _ _ _ _) (Single _ _ _ _ (Just Dnf) _ _) = LT
    compare s1 s2 = (singleTime s1) `compare` (singleTime s2)

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

data SubmittedSingle = SubmittedSingle
    { submittedSingleScramble :: !Text
    , submittedSingleTime :: !DurationInMs
    , submittedSinglePenalty :: !(Maybe Penalty)
    } deriving (Show)

instance FromJSON SubmittedSingle where
    parseJSON (Object v) = SubmittedSingle <$>
                             v .: "scramble" <*>
                             v .: "time" <*>
                             v .:? "penalty"
    parseJSON _          = mempty

data RecordSingle = RecordSingle
    { recordSingleId :: SingleId
    , recordSingleTime :: DurationInMs
    , recordSingleScramble :: Text
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
    , simpleUserSlug :: UserSlug
    , simpleUserName :: Text
    , simpleUserSinglesCount :: Int
    }

instance ToJSON SimpleUser where
    toJSON SimpleUser{..} = object
      [ "id" .= simpleUserId
      , "slug" .= simpleUserSlug
      , "name" .= simpleUserName
      , "singles_count" .= simpleUserSinglesCount
      ]

instance FromRow SimpleUser where
    fromRow = SimpleUser <$> field <*> field <*> field <*> field

data UserRole = AdminRole | ModeratorRole | UserRole | BetaUserRole deriving (Eq, Show)
instance FromField UserRole where
    fromField f s = do
        s <- fromField f s
        case (s :: String) of
            "user" -> pure UserRole
            "moderator" -> pure ModeratorRole
            "admin" -> pure AdminRole
            "beta_user" -> pure BetaUserRole
            _ -> mzero

data User = User
    { userId :: UserId
    , userName :: Text
    , userSlug :: UserSlug
    , userEmail :: Text
    , userRole :: UserRole
    , userWca :: Maybe Text
    , userIgnored :: Bool
    , userWastedTime :: Integer
    } deriving (Show)

instance Eq User where
    (==) u1 u2 = userId u1 == userId u2

instance ToJSON User where
    toJSON User{..} = object
        [ "id" .= userId
        , "name" .= userName
        , "slug" .= userSlug
        , "wca" .= userWca
        ]

hasWcaId :: User -> Bool
hasWcaId = isJust . userWca

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field <*> field <*> wcaField <*> field <*> field
      where
      -- converting empty string to Nothing. :(
      wcaField = do
          text <- field
          case text of
            Just t -> if t == "" then return Nothing else return $ Just t
            Nothing -> return Nothing

newtype LoggedIn a = LoggedIn { getLoggedIn :: a } deriving (Show)
instance ToJSON a => ToJSON (LoggedIn a) where
    toJSON (LoggedIn x) = toJSON x

type LoggedInUser = LoggedIn User

data RecordType = SingleRecord | AverageOf5Record | AverageOf12Record deriving (Enum, Bounded, Ord, Eq)

shortRecordTypeName :: RecordType -> Text
shortRecordTypeName SingleRecord = "Single"
shortRecordTypeName AverageOf5Record = "Avg5"
shortRecordTypeName AverageOf12Record = "Avg12"

instance ToMarkup RecordType where
    toMarkup SingleRecord = toMarkup ("Single" :: Text)
    toMarkup AverageOf5Record = toMarkup ("Average of 5" :: Text)
    toMarkup AverageOf12Record = toMarkup ("Average of 12" :: Text)

allRecordTypes :: [RecordType]
allRecordTypes = [minBound..]

instance ToField RecordType where
    toField SingleRecord = toField (1 :: Int)
    toField AverageOf5Record = toField (5 :: Int)
    toField AverageOf12Record = toField (12 :: Int)
instance FromField RecordType where
    fromField f s = do
        a <- fromField f s
        case (a :: Int) of
          1  -> return SingleRecord
          5  -> return AverageOf5Record
          12 -> return AverageOf12Record
          _  -> mzero

instance ToJSON RecordType where
    toJSON SingleRecord = String "Single"
    toJSON AverageOf5Record = String "Average of 5"
    toJSON AverageOf12Record = String "Average of 12"

instance FromHttpApiData RecordType where
    parseUrlPiece i = do
        s <- parseUrlPiece i
        case (s :: String) of
          "single" -> Right SingleRecord
          "avg5" -> Right AverageOf5Record
          "avg12" -> Right AverageOf12Record
          _ -> Left "is not a valid RecordType"

instance ToHttpApiData RecordType where
    toQueryParam SingleRecord = toQueryParam ("single" :: String)
    toQueryParam AverageOf5Record = toQueryParam ("avg5" :: String)
    toQueryParam AverageOf12Record = toQueryParam ("avg12" :: String)


data Record = Record
    { recordId :: Int
    , recordTime :: DurationInMs
    , recordComment :: String
    , recordPuzzleId :: PuzzleId
    , recordUserId :: UserId
    , recordType :: RecordType
    , recordSetAt :: UTCTime
    , recordSingles :: [RecordSingle] -- TODO: remove me/use tuple for joins
    }

instance FromRow Record where
    fromRow = Record <$> field <*> field <*> field <*> field <*> field <*> field <*> (localTimeToUTC utc <$> field) <*> pure []

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

data Puzzle = Puzzle
    { puzzleId :: PuzzleId
    , puzzleName :: Text
    , puzzleCssPosition :: Int
    , puzzleSlug :: PuzzleSlug
    , puzzleKindId :: KindId
    } deriving (Eq)

instance FromRow Puzzle where
    fromRow = Puzzle <$> field <*> field <*> field <*> field <*> field

instance Ord Puzzle where
    left `compare` right =
        let toTuple p = (puzzleName p, puzzleId p, puzzleCssPosition p)
        in toTuple left `compare` toTuple right
instance ToJSON Puzzle where
    toJSON Puzzle{..} = object
        [ "id" .= puzzleId
        , "name" .= puzzleName
        , "css_position" .= puzzleCssPosition
        , "slug" .= puzzleSlug
        , "kind_id" .= puzzleKindId
        ]


data Kind = Kind
    { kindId :: Int
    , kindName :: Text
    , kindShortName :: Maybe Text
    , kindCssPosition :: Int
    } deriving (Eq)

instance ToJSON Kind where
    toJSON Kind{..} = object
        [ "id" .= kindId
        , "name" .= kindName
        , "short_name" .= kindShortName
        , "css_position" .= kindCssPosition
        ]

instance Ord Kind where
    left `compare` right =
        let toTuple k = (kindShortName k, kindName k, kindId k, kindCssPosition k)
        in toTuple left `compare` toTuple right

instance FromRow Kind where
    fromRow = Kind <$> field <*> field <*> field <*> field

data PuzzleWithNestedKind = PuzzleWithNestedKind Puzzle Kind
instance ToJSON PuzzleWithNestedKind where
    toJSON (PuzzleWithNestedKind Puzzle{..} k) = object
        [ "id" .= puzzleId
        , "name" .= puzzleName
        , "css_position" .= puzzleCssPosition
        , "slug" .= puzzleSlug
        , "kind" .= (toJSON k)
        ]

data ChartData = ChartData
    { chartTime :: Double
    , chartComment :: Maybe Text
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

newtype Activity = Activity (Map.Map LocalTime Int) deriving (Show)

instance ToJSON Activity where
    -- TODO: order?
    toJSON (Activity m) = object $ (\(k, v) -> (pack $ show k) .= v) <$> Map.toAscList m


data SubmittedComment = SubmittedComment
    { submittedCommentContent :: Text
    } deriving (Show)

newtype LocalTimeWithFromRow = LocalTimeWithFromRow LocalTime deriving (Generic)
instance FromRow LocalTimeWithFromRow

localTimeToUTCTime :: LocalTimeWithFromRow -> UTCTime
localTimeToUTCTime (LocalTimeWithFromRow t) = localTimeToUTC utc t
