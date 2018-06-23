{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import Prelude hiding (id)
import GHC.Generics
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.=), object, (.:), (.:?))
import Servant (FromHttpApiData(..))
import Web.HttpApiData (ToHttpApiData, toQueryParam)
import Data.Text (Text, pack)
import Data.String (IsString, fromString)
import Data.Char (chr)
import Data.Time.LocalTime (LocalTime, localTimeToUTC, utc)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Clock (UTCTime)
import Data.Monoid ((<>))
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import Data.UUID (UUID, toText)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
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

newtype PuzzleId = PuzzleId Int deriving (Generic, Eq, Ord, Show)
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

newtype KindId = KindId Int deriving (Eq, Generic, Ord)
instance FromField KindId where
    fromField f s = KindId <$> fromField f s
instance ToField KindId where
    toField (KindId id) = toField id
instance ToJSON KindId

-- TODO: Create generic slug data type, makes slug generation easier.
-- newtype Slug a = Slug Text deriving (Eq, Generic, Ord)

data Limit = Limit Int | NoLimit deriving (Generic)
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
instance ToField SingleId where
    toField (SingleId id) = toField id
instance FromHttpApiData SingleId where
  parseUrlPiece s = SingleId <$> parseUrlPiece s
instance ToJSON SingleId

newtype UserId = UserId Int deriving (Generic, Show, Eq, Ord)
instance FromField UserId where
    fromField f s = UserId <$> fromField f s
instance FromRow UserId
instance ToJSON UserId
instance FromJSON UserId
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

data SubmittedAnnouncement = SubmittedAnnouncement
    { submittedAnnouncementTitle :: Text
    , submittedAnnouncementContent :: Text
    }

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
    , singlePuzzleId :: !PuzzleId
    } deriving (Show, Eq)

isDnf :: Single -> Bool
isDnf (Single _ _ _ _ (Just Dnf) _ _ _) = True
isDnf _ = False

instance Ord Single where
    compare (Single _ _ _ _ (Just Dnf) _ _ _) (Single _ _ _ _ (Just Dnf) _ _ _) = EQ
    compare (Single _ _ _ _ (Just Dnf) _ _ _) Single{}  = GT
    compare Single{} (Single _ _ _ _ (Just Dnf) _ _ _) = LT
    compare s1 s2 = singleTime s1 `compare` singleTime s2

instance ToJSON Single where
    toJSON Single{..} = object
      [ "id" .= singleId
      , "time" .= singleTime
      , "comment" .= singleComment
      , "scramble" .= singleScramble
      , "penalty" .= singlePenalty
      , "created_at" .= singleCreatedAt
      ]



instance FromRow Single where
    fromRow = Single <$> field <*> field <*> field <*> field <*> field <*> ((localTimeToUTC utc) <$> field) <*> field <*> field

data SubmittedSingle = SubmittedSingle
    { submittedSingleScramble :: !Text
    , submittedSingleTime :: !DurationInMs
    , submittedSinglePenalty :: !(Maybe Penalty)
    , submittedSingleComment :: !(Maybe Text)
    } deriving (Show)

instance FromJSON SubmittedSingle where
    parseJSON (Object v) = SubmittedSingle <$>
                             v .: "scramble" <*>
                             v .: "time" <*>
                             v .:? "penalty" <*>
                             v .:? "comment"
    parseJSON _          = mempty

newtype RecordWithSingles = RecordWithSingles (DbEntry Record, [Single])

instance ToJSON RecordWithSingles where
    toJSON (RecordWithSingles (DbEntry id (Record{..}), singles)) = object
        [ "id" .= id
        , "time" .= recordTime
        , "set_at" .= (0 :: Int) -- TODO: fix me
        , "comment" .=  recordComment
        , "puzzle_id" .= recordPuzzleId
        , "type_full_name" .= recordType
        , "singles" .= singles
        ]

newtype ClearPassword = ClearPassword Text deriving (Eq)
instance Show ClearPassword where
    show _ = ""
newtype HashedPassword = HashedPassword BS.ByteString deriving (Eq, Show)

instance FromField HashedPassword where
    fromField f s = HashedPassword <$> fromField f s
instance ToField HashedPassword where
    toField (HashedPassword pw) = toField pw

newtype Salt = Salt BS.ByteString deriving (Show)

instance FromField Salt where
    fromField f s = Salt <$> fromField f s
instance ToField Salt where
    toField (Salt s) = toField s

newtype Email = Email Text deriving (Show, Eq)

instance ToField Email where
    toField (Email t) = toField t
instance FromField Email where
    fromField f s = Email <$> fromField f s

data SubmittedUser = SubmittedUser
    { submittedUserName :: Text
    , submittedUserEmail :: Email
    , submittedUserWca :: Text
    , submittedUserTimeZone :: Text
    , submittedPassword :: ClearPassword
    } deriving Show

data SubmittedEditUser = SubmittedEditUser
    { submittedEditUserName :: Text
    , submittedEditUserEmail :: Email
    , submittedEditUserWca :: Text
    , submittedEditUserTimeZone :: Text
    , submittedEditUserPassword :: Maybe ClearPassword
    , submittedEditUserIgnored :: Maybe Bool
    }

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
    , userEmail :: Email
    , userRole :: UserRole
    , userWca :: Maybe Text
    , userIgnored :: Bool
    , userWastedTime :: Integer
    , userSalt :: Salt
    , userPassword :: HashedPassword
    , userTimeZone :: Text
    } deriving (Show)

loggedInUserIsAdmin :: LoggedIn User -> Bool
loggedInUserIsAdmin (LoggedIn u _) = userIsAdmin u

userIsAdmin :: User -> Bool
userIsAdmin User{..} = userRole == AdminRole

instance Eq User where
    (==) u1 u2 = userId u1 == userId u2

instance ToJSON User where
    toJSON User{..} = object
        [ "id" .= userId
        , "name" .= userName
        , "slug" .= userSlug
        , "wca" .= userWca
        ]

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field <*> field <*> wcaField <*> field <*> field <*> field <*> field <*> field
      where
      -- converting empty string to Nothing. :(
      wcaField = do
          text <- field
          case text of
            Just t -> if t == "" then return Nothing else return $ Just t
            Nothing -> return Nothing

-- TODO: Rename to SessionData?
data LoggedIn a = LoggedIn { getLoggedIn :: a, getSessionId :: SessionId }

type LoggedInUser = LoggedIn User

data SubmittedLogin = SubmittedLogin
    { submittedLoginName :: Text
    , submittedLoginPassword :: ClearPassword
    }

data SubmittedResetPassword = SubmittedResetPassword
    { submittedResetPasswordEmail :: Email
    }

data RecordType
    = SingleRecord
    | AverageOf5Record
    | AverageOf12Record
    deriving (Enum, Bounded, Ord, Eq, Show)

singleCount :: RecordType -> Int
singleCount SingleRecord      = 1
singleCount AverageOf5Record  = 5
singleCount AverageOf12Record = 12

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


newtype Id a = Id Int deriving (Show, Generic)
instance ToJSON (Id a)
instance FromField (Id a) where
    fromField f s = Id <$> fromField f s
instance ToField (Id a) where
    toField (Id id) = toField id
instance FromRow (Id a) where
    fromRow = Id <$> field
instance FromHttpApiData (Id a) where
    parseUrlPiece t = Id <$> parseUrlPiece t

data DbEntry a = DbEntry (Id a) a
dbEntryId (DbEntry id _) = id
dbEntryRow (DbEntry _ r) = r

instance Show a => Show (DbEntry a) where
    show (DbEntry (Id id) x) = "DbEntry (Id " ++ show id ++ ") (" ++ show x ++ ")"

-- TODO: Use DbEntity a = DbEntity (Id a) a
data Record = Record
    { recordTime :: !DurationInMs
    , recordComment :: !String
    , recordPuzzleId :: !PuzzleId
    , recordUserId :: !UserId
    , recordType :: !RecordType
    , recordSetAt :: !UTCTime
    } deriving (Show)

instance ToRow Record where
    toRow Record{..} =
        [ toField recordTime,
          toField recordComment,
          toField recordPuzzleId,
          toField recordUserId,
          toField recordType,
          toField recordSetAt
        ]

instance FromRow (DbEntry Record) where
    fromRow = do
        id <- field
        inner <- Record <$> field <*> field <*> field <*> field <*> field <*> (localTimeToUTC utc <$> field)
        pure $ DbEntry id inner



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
    { kindId :: KindId
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

fullPuzzleName :: (Puzzle, Kind) -> Text
fullPuzzleName (Puzzle{..}, Kind{..}) =
    case kindShortName of
        Just sName -> puzzleName <> " " <> sName
        Nothing -> puzzleName

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

newtype SessionId = SessionId UUID

instance ToField SessionId where
    toField (SessionId uuid) = toField $ toText uuid

data SerializedSessionData = SerializedSessionData
    { sessionDataUserId :: UserId
    } deriving (Generic)

instance FromJSON SerializedSessionData
instance ToJSON SerializedSessionData

newtype FlashMessage = FlashMessage Text deriving (Show)
instance ToMarkup FlashMessage where
    toMarkup (FlashMessage t) = toMarkup t

instance IsString FlashMessage where
    fromString s = FlashMessage $ pack s
