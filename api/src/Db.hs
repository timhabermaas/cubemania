{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE BangPatterns               #-}

module Db
    ( matchUsers
    , withPool
    , getRecordForUserAndPuzzleAndType
    , getRecordById
    , getRecords
    , getRecordsForPuzzleAndType
    , getRecordCountForPuzzleAndType
    , deleteAllRecordsForUserAndPuzzle
    , saveRecord
    , getSingles
    , getSingle
    , runDb
    , getChartData
    , getUsers
    , getUserBySlug
    , getUserById
    , getUserByName
    , getUserByEmail
    , updateUserPassword
    , getRecordsForUser
    , maxSinglesCount
    , postSingle
    , deleteSingle
    , updateSingle
    , getLatestAnnouncement
    , getAnnouncement
    , getAnnouncements
    , getCommentsForAnnouncement
    , postComment
    , postAnnouncement
    , createUser
    , updateAnnouncement
    , getActivity
    , getAllSingles
    , getPuzzleKindById
    , getPuzzleBySlug
    , getKindById
    , getAllPuzzles
    , createSession
    , deleteSession
    , readSession
    , withTransaction
    ) where

import Prelude hiding (id)
import Types
import Utils
import Types.Configuration
import Data.Monoid ((<>))
import Data.Pool
import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime, utctDay)
import Data.Time.Calendar (addDays)
import Data.Int (Int64)
import Data.Aeson (encode, decode)
import System.Random
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Database.PostgreSQL.Simple (Connection, query, query_, formatQuery, formatMany, fold_, execute, executeMany, Only(..), Query, In(..))
import qualified Database.PostgreSQL.Simple as PSQL
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (fromStrict)
import Data.Pool (withResource)

runDb :: (MonadReader Configuration m, MonadIO m) => (Connection -> IO a) -> m a
runDb q = do
    pool <- asks getPool
    liftIO $ withResource pool q

withPool :: (MonadIO m) => Pool Connection -> (Connection -> IO a) -> m a
withPool pool action = liftIO $ withResource pool $ action

getSingles :: (MonadIO m) => PuzzleId -> UserId -> Limit -> Connection -> m [Single]
getSingles (PuzzleId pid) (UserId uid) l conn =
    case l of
        Limit limit ->
            myQuery conn "select id, time, comment, scramble, penalty, created_at, user_id, puzzle_id from singles where puzzle_id = ? and user_id = ? ORDER BY created_at DESC LIMIT ?" (pid, uid, limit)
        NoLimit ->
            myQuery conn "select id, time, comment, scramble, penalty, created_at, user_id, puzzle_id from singles where puzzle_id = ? and user_id = ? ORDER BY created_at DESC" (pid, uid)

getSingle :: (MonadIO m) => SingleId -> Connection -> m Single
getSingle (SingleId id) conn = do
    result <- myQuery conn "SELECT id, time, comment, scramble, penalty, created_at, user_id, puzzle_id FROM singles WHERE id = ?" (Only id)
    case safeHead result of
      Just x -> return x
      Nothing -> error "nope"

getRecordForUserAndPuzzleAndType :: (MonadIO m) => UserId -> PuzzleId -> RecordType -> Connection -> m (Maybe (DbEntry Record))
getRecordForUserAndPuzzleAndType userId puzzleId recordType conn = do
    safeHead <$> myQuery conn "SELECT id, time, comment, puzzle_id, user_id, amount, set_at FROM records WHERE user_id = ? AND puzzle_id = ? AND amount = ?" (userId, puzzleId, recordType)

getRecordById :: (MonadIO m) => (Id Record) -> Connection -> m (Maybe (DbEntry Record, [Single]))
getRecordById recordId conn = do
    records <- myQuery conn "SELECT id, time, comment, puzzle_id, user_id, amount, set_at FROM records WHERE id = ?" (Only recordId)
    case safeHead records of
        Just record@(DbEntry rId _) -> do
            singles <- liftIO $ grabSingles conn rId
            pure $ Just (record, singles)
        Nothing -> pure Nothing

getRecords :: (MonadIO m) => UserId -> PuzzleId -> Connection -> m [(DbEntry Record, [Single])]
getRecords (UserId uid) (PuzzleId pid) conn = do
    records <- myQuery conn "select id, time, comment, puzzle_id, user_id, amount, set_at from records where user_id = ? and puzzle_id = ?" (uid, pid)
    mapM (\r@(DbEntry rId _) ->  (,) r <$> grabSingles conn rId) records

getRecordsForPuzzleAndType :: (MonadIO m) => PuzzleId -> RecordType -> Int -> Connection -> m [(DbEntry Record, SimpleUser)]
getRecordsForPuzzleAndType pId type' page conn = do
    let offset = (page - 1) * 50
    foo <- myQuery conn "SELECT records.id, records.time, records.comment, records.puzzle_id, records.user_id, records.amount, records.set_at, users.id, users.slug, users.name, users.singles_count FROM records LEFT OUTER JOIN users ON users.id = records.user_id WHERE records.puzzle_id = ? AND records.amount = ? AND users.ignored = 'f' ORDER BY records.time OFFSET ? LIMIT 50" (pId, type', offset)
    return $ unwrapJoinedResult2 <$> foo

getRecordCountForPuzzleAndType :: (MonadIO m) => PuzzleId -> RecordType -> Connection -> m Int
getRecordCountForPuzzleAndType pId type' conn = do
    [Only i] <- myQuery conn "SELECT COUNT(DISTINCT records.id) FROM records LEFT OUTER JOIN users ON users.id = records.user_id WHERE records.puzzle_id = ? AND users.ignored = 'f' AND records.amount = ?" (pId, type')
    return i

deleteAllRecordsForUserAndPuzzle :: (MonadIO m) => UserId -> PuzzleId -> Connection -> m ()
deleteAllRecordsForUserAndPuzzle uId pId conn = do
    records <- getRecords uId pId conn
    let recordIds = dbEntryId . fst <$> records
    myExecute conn "DELETE FROM records_singles WHERE record_id IN ?" (Only $ In recordIds)
    myExecute conn "DELETE FROM records WHERE id IN ?" (Only $ In recordIds)
    pure ()

saveRecord :: (MonadIO m) => Record -> [Single] -> Connection -> m (Id Record)
saveRecord Record{..} singles conn = do
    time <- liftIO getCurrentTime
    liftIO $ withTransaction conn $ do
        [recordId :: Id Record] <- myQuery conn "INSERT INTO records (time, comment, puzzle_id, user_id, amount, set_at, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT (puzzle_id, amount, user_id) DO UPDATE SET time = EXCLUDED.time, comment = EXCLUDED.comment, set_at = EXCLUDED.set_at, created_at = EXCLUDED.created_at, updated_at = EXCLUDED.updated_at RETURNING id" (recordTime, recordComment, recordPuzzleId, recordUserId, recordType, recordSetAt, time, time)
        let recordSinglePairs = (\Single{..} -> (recordId, singleId)) <$> singles
        -- Delete all existing single -> record associations for that record.
        myExecute conn "DELETE FROM records_singles WHERE record_id = ?" (Only recordId)
        myExecuteMany conn "INSERT INTO records_singles (record_id, single_id) VALUES (?, ?)" recordSinglePairs
        pure recordId

grabSingles :: (MonadIO m) => Connection -> Id Record -> m [Single]
grabSingles conn rId = do
    myQuery conn "SELECT singles.id, singles.time, singles.comment, singles.scramble, singles.penalty, singles.created_at, singles.user_id, singles.puzzle_id FROM singles INNER JOIN records_singles ON singles.id = records_singles.single_id WHERE records_singles.record_id = ? ORDER BY singles.created_at" (Only rId)

postSingle :: (MonadIO m) => PuzzleId -> UserId -> SubmittedSingle -> Connection -> m SingleId
postSingle (PuzzleId pid) (UserId userId) (SubmittedSingle s t _p) conn = do
    result :: [Only Int] <- liftIO $ withTransaction conn $ do
        time <- getCurrentTime
        _ <- myExecute conn "UPDATE users SET singles_count = COALESCE(singles_count, 0) + 1 WHERE users.id = ?" (Only userId)
        myQuery conn "INSERT INTO singles (time, puzzle_id, user_id, scramble, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?) RETURNING id" (t, pid, userId, s, time, time)
    case safeHead result of
        Just (Only x) -> return $ SingleId $ x
        Nothing -> error "not gonna happen"

updateSingle :: (MonadIO m) => SingleId -> SubmittedSingle -> Connection -> m SingleId
updateSingle (SingleId sid) s conn = do
    time' <- liftIO getCurrentTime
    myExecute conn "UPDATE singles SET time=?, updated_at=?, penalty=? WHERE id = ?" (submittedSingleTime s, time', submittedSinglePenalty s, sid)
    return $ SingleId sid

deleteSingle :: (MonadIO m) => SingleId -> Connection -> m ()
deleteSingle s@(SingleId singleId) conn = do
    liftIO $ withTransaction conn $ do
        (UserId userId) <- singleUserId <$> getSingle s conn
        _ <- myExecute conn "UPDATE users SET singles_count = COALESCE(singles_count, 0) - 1 WHERE users.id = ?" (Only userId)
        _ <- myExecute conn "DELETE FROM singles WHERE id = ?" (Only singleId)
        return ()

matchUsers :: (MonadIO m) => Text -> Connection -> m [SimpleUser]
matchUsers q conn =
    myQuery conn "SELECT id, slug, name, singles_count FROM users WHERE lower(name) LIKE ? ORDER BY singles_count DESC LIMIT 200" (Only $ "%" <> q <> "%")

getUserBySlug :: (MonadIO m) => UserSlug -> Connection -> m (Maybe User)
getUserBySlug slug conn = do
    users <- myQuery conn "SELECT id, name, slug, email, role, wca, ignored, wasted_time, salt, encrypted_password FROM users WHERE slug = ?" (Only slug)
    return $ safeHead users

getUserByName :: (MonadIO m) => Text -> Connection -> m (Maybe User)
getUserByName name conn = do
    users <- myQuery conn "SELECT id, name, slug, email, role, wca, ignored, wasted_time, salt, encrypted_password FROM users WHERE lower(name) = lower(?)" (Only name)
    return $ safeHead users

getUserByEmail :: (MonadIO m) => Email -> Connection -> m (Maybe User)
getUserByEmail (Email email) conn = do
    users <- myQuery conn "SELECT id, name, slug, email, role, wca, ignored, wasted_time, salt, encrypted_password FROM users WHERE lower(email) = lower(?)" (Only email)
    return $ safeHead users

getUserById :: (MonadIO m) => UserId -> Connection -> m (Maybe User)
getUserById id conn = do
    users <- myQuery conn "SELECT id, name, slug, email, role, wca, ignored, wasted_time, salt, encrypted_password FROM users WHERE id = ?" (Only id)
    return $ safeHead users

updateUserPassword :: (MonadIO m) => UserId -> Salt -> HashedPassword -> Connection -> m ()
updateUserPassword id salt pw conn = do
    myExecute conn "UPDATE users SET salt = ?, encrypted_password = ? WHERE id = ?" (salt, pw, id)
    return ()

getAnnouncement :: (MonadIO m) => AnnouncementId -> Connection -> m (Maybe Announcement)
getAnnouncement id conn = do
    posts <- myQuery conn "SELECT id, title, content, user_id, created_at FROM posts WHERE id = ?" (Only id)
    return $ safeHead posts

getAnnouncements :: (MonadIO m) => Connection -> m [Announcement]
getAnnouncements conn =
    myQuery_ conn "SELECT id, title, content, user_id, created_at FROM posts ORDER BY created_at DESC"

getCommentsForAnnouncement :: (MonadIO m) => AnnouncementId -> Connection -> m [Comment]
getCommentsForAnnouncement id conn = do
    myQuery conn "SELECT id, content, user_id, created_at FROM comments WHERE commentable_id = ? AND commentable_type = 'Post' ORDER BY created_at" (Only id)

data JoinedResult2 a b = JoinedResult2 { unwrapJoinedResult2 :: (a, b) }
instance (FromRow a, FromRow b) => FromRow (JoinedResult2 a b) where
    fromRow = JoinedResult2 <$> ((,) <$> fromRow <*> fromRow)
data JoinedRecordResult = JoinedRecordResult { unwrapJoinedRecordResult :: (Puzzle, Kind, DbEntry Record) }

instance FromRow JoinedRecordResult where
    fromRow = JoinedRecordResult <$> ((,,) <$> fromRow <*> fromRow <*> fromRow)

getRecordsForUser :: (MonadIO m) => UserId -> Connection -> m (Map.Map (Puzzle, Kind) (Map.Map RecordType DurationInMs))
getRecordsForUser uid conn = do
    result <- myQuery conn "SELECT puzzles.id, puzzles.name, puzzles.css_position, puzzles.slug, puzzles.kind_id, kinds.id, kinds.name, kinds.short_name, kinds.css_position, records.id, records.time, records.comment, records.puzzle_id, records.user_id, records.amount, records.set_at FROM records LEFT OUTER JOIN puzzles ON puzzles.id = records.puzzle_id LEFT OUTER JOIN kinds ON kinds.id = puzzles.kind_id WHERE records.user_id = ? ORDER BY puzzles.name, kinds.name" (Only uid)
    return $ groupByPuzzle (unwrapJoinedRecordResult <$> result)
  where
    groupByPuzzle :: [(Puzzle, Kind, DbEntry Record)] -> Map.Map (Puzzle, Kind) (Map.Map RecordType DurationInMs)
    groupByPuzzle i = Map.fromListWith
                        Map.union
                        [((p, k), Map.singleton (recordType) (recordTime)) | (p, k, DbEntry _ Record{..}) <- i]

getUsers :: (MonadIO m) => Int -> Connection -> m [SimpleUser]
getUsers page conn = do
    let offset = (page - 1) * 200
    myQuery conn "SELECT id, slug, name, singles_count FROM users ORDER BY singles_count DESC LIMIT 200 OFFSET ?" (Only offset)

maxSinglesCount :: (MonadIO m) => Connection -> m (Maybe Int)
maxSinglesCount conn = do
    counts <- myQuery_ conn "SELECT singles_count FROM users ORDER BY singles_count DESC LIMIT 1"
    case counts of
      [Only x] -> return $ Just x
      _        -> return Nothing

fromMaybeM :: (Monad m) => m a -> Maybe a -> m a
fromMaybeM _ (Just x) = return x
fromMaybeM def Nothing = def

-- TODO: Make (Maybe UTCTime, Maybe UTCTime) to Maybe (UTCTime, UTCTime) ?
getChartData :: (MonadIO m) => PuzzleId -> UserId -> (Maybe UTCTime, Maybe UTCTime) -> Connection -> m [ChartData]
getChartData (PuzzleId puzzleId) (UserId userId) (from, to) conn = do
    startDate <- liftIO $ fromMaybeM firstSingleDate from
    endDate <- liftIO $ fromMaybeM getCurrentTime to
    let difference = diffUTCTime endDate startDate
    case groupingForDateDiff difference of
        Just group -> liftIO $ groupSingles (startDate, endDate) group
        Nothing -> liftIO $ fetchSingles (startDate, endDate)
  where
    groupingForDateDiff :: NominalDiffTime -> Maybe Text
    groupingForDateDiff diff =
        let
            diffInDays = (toRational diff) / 60.0 / 60.0 / 24.0
        in
            if diffInDays > 365 * 2 then
              Just "month"
            else if diffInDays > 200 then
              Just "week"
            else if diffInDays > 30 then
              Just "day"
            else
              Nothing
    groupSingles :: (UTCTime, UTCTime) -> Text -> IO [ChartData]
    groupSingles (from', to') grouping = myQuery conn "SELECT AVG(time) as time, string_agg(comment, '\n') AS comment, date_trunc(?, created_at) as created_at FROM singles WHERE (penalty IS NULL OR penalty NOT LIKE 'dnf') AND puzzle_id = ? AND user_id = ? AND created_at BETWEEN ? AND ? GROUP BY date_trunc(?, created_at) ORDER BY created_at" (grouping, puzzleId, userId, from', to', grouping)
    fetchSingles :: (UTCTime, UTCTime) -> IO [ChartData]
    fetchSingles (from', to') = myQuery conn "SELECT time, comment, created_at FROM singles WHERE (penalty IS NULL OR penalty NOT LIKE 'dnf') AND puzzle_id = ? AND user_id = ? AND created_at BETWEEN ? AND ? ORDER BY created_at" (puzzleId, userId, from', to')
    firstSingleDate :: IO UTCTime
    firstSingleDate = (localTimeToUTCTime . head) <$> myQuery conn "SELECT created_at FROM singles WHERE puzzle_id = ? AND user_id = ? ORDER BY created_at LIMIT 1" (puzzleId, userId)

getLatestAnnouncement :: (MonadIO m) => Connection -> m (Maybe Announcement)
getLatestAnnouncement conn = do
    posts <- myQuery_ conn "SELECT id, title, content, user_id, created_at FROM posts ORDER BY created_at desc LIMIT 1"
    return $ safeHead posts

postComment :: (MonadIO m) => AnnouncementId -> UserId -> Text -> Connection -> m ()
postComment pId uId content conn = do
    time <- liftIO getCurrentTime
    _ <- myExecute conn "INSERT INTO comments (content, user_id, commentable_id, commentable_type, created_at) VALUES (?, ?, ?, 'Post', ?)" (content, uId, pId, time)
    return ()

postAnnouncement :: (MonadIO m) => UserId -> SubmittedAnnouncement -> Connection -> m AnnouncementId
postAnnouncement uId SubmittedAnnouncement{..} conn = do
    time <- liftIO getCurrentTime
    [(Only id)] <- myQuery conn "INSERT INTO posts (title, content, user_id, created_at, updated_at) VALUES (?, ?, ?, ?, ?) RETURNING id" (submittedAnnouncementTitle, submittedAnnouncementContent, uId, time, time)
    pure $ AnnouncementId id

createUser :: (MonadIO m) => SubmittedUser -> Salt -> HashedPassword -> Connection -> m UserId
createUser SubmittedUser{..} salt pw conn = do
    time <- liftIO getCurrentTime
    [(Only id)] <- myQuery conn "INSERT INTO users (name, slug, email, salt, encrypted_password, time_zone, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?) RETURNING id" (submittedUserName, submittedUserName, submittedUserEmail, salt, pw, submittedUserTimeZone, time, time)
    pure $ UserId id

-- Split into updating password and setting user information
updateUser :: (MonadIO m) => SubmittedEditUser -> Maybe Salt -> Maybe HashedPassword -> Connection -> m UserId
updateUser = undefined

updateAnnouncement :: (MonadIO m) => AnnouncementId -> SubmittedAnnouncement -> Connection -> m ()
updateAnnouncement aId SubmittedAnnouncement{..} conn = do
    time <- liftIO getCurrentTime
    _ <- myExecute conn "UPDATE posts SET title=?, content=?, updated_at=? WHERE id=?" (submittedAnnouncementTitle, submittedAnnouncementContent, time, aId)
    return ()

getActivity :: (MonadIO m) => UserId -> Connection -> m (Activity)
getActivity uid conn = do
    today <- liftIO $ utctDay <$> getCurrentTime
    let oneYearAgo = addDays (-366) today
    result <- myQuery conn "SELECT date_trunc('day', created_at), COUNT(*) FROM singles WHERE user_id = ? AND (created_at BETWEEN ? AND ?) GROUP BY date_trunc('day', created_at)" (uid, oneYearAgo, today)
    return $ Activity $ Map.fromList $ fmap (\(t, x) -> (t, x)) result

getAllSingles :: (MonadIO m) => (Single -> IO ()) -> Connection -> m ()
getAllSingles callback conn =
    liftIO $ withTransaction conn $
        liftIO $ fold_ conn "select id, time, comment, scramble, penalty, created_at, user_id, puzzle_id from singles ORDER BY created_at" () (\() !single -> callback single)

getPuzzleBySlug :: (MonadIO m) => PuzzleSlug -> Connection -> m (Maybe Puzzle)
getPuzzleBySlug pSlug conn = do
    puzzles <- myQuery conn "SELECT id, name, css_position, slug, kind_id FROM puzzles WHERE slug = ?" (Only pSlug)
    pure $ safeHead puzzles

getPuzzleById :: (MonadIO m) => PuzzleId -> Connection -> m (Maybe Puzzle)
getPuzzleById puzzleId conn = do
    puzzles <- myQuery conn "SELECT id, name, css_position, slug, kind_id FROM puzzles WHERE id = ?" (Only puzzleId)
    pure $ safeHead puzzles

getPuzzleKindById :: (MonadIO m) => PuzzleId -> Connection -> m (Maybe (Puzzle, Kind))
getPuzzleKindById puzzleId conn = do
    puzzle <- getPuzzleById puzzleId conn
    -- TODO: MaybeT?
    case puzzle of
        Just p -> do
            kind <- getKindById (puzzleKindId p) conn
            case kind of
                Just k -> pure $ Just (p, k)
                Nothing -> pure Nothing
        Nothing -> pure Nothing

getKindById :: (MonadIO m) => KindId -> Connection -> m (Maybe Kind)
getKindById kId conn = do
    kinds <- myQuery conn "SELECT id, name, short_name, css_position FROM kinds WHERE id = ?" (Only kId)
    pure $ safeHead kinds

getAllPuzzles :: (MonadIO m) => Connection -> m [(Kind, Puzzle)]
getAllPuzzles conn = do
    foo <- myQuery_ conn "SELECT kinds.id, kinds.name, kinds.short_name, kinds.css_position, puzzles.id, puzzles.name, puzzles.css_position, puzzles.slug, puzzles.kind_id FROM puzzles LEFT OUTER JOIN kinds ON kinds.id = puzzles.kind_id ORDER BY puzzles.name"
    return $ unwrapJoinedResult2 <$> foo

createSession :: (MonadIO m) => SerializedSessionData -> Connection -> m SessionId
createSession sessionData conn = do
    sessionId <- liftIO (SessionId <$> randomIO)
    time <- liftIO getCurrentTime
    _ <- myExecute conn "INSERT INTO sessions (session_id, data, created_at, updated_at) VALUES (?, ?, ?, ?)" (sessionId, encode sessionData, time, time)
    return sessionId

deleteSession :: (MonadIO m) => SessionId -> Connection -> m ()
deleteSession sessionId conn = do
    myExecute conn "DELETE FROM sessions WHERE session_id = ?" (Only sessionId)
    return ()

readSession :: (MonadIO m) => SessionId -> Connection -> m (Maybe SerializedSessionData)
readSession sessionId conn = do
    sessionData <- myQuery conn "SELECT data FROM sessions WHERE session_id = ?" (Only sessionId)
    case safeHead sessionData of
        Just (Only text) -> do
            -- TODO: Add FrowField SessionData instance which converts directly from JSON text to value.
            return $ decode (fromStrict (encodeUtf8 text))
        Nothing ->
            return Nothing

myQuery :: (ToRow q, FromRow r, MonadIO m) => Connection -> Query -> q -> m [r]
myQuery conn q params = liftIO $ do
    formatQuery conn q params >>= print
    query conn q params

myQuery_ :: (FromRow r, MonadIO m) => Connection -> Query -> m [r]
myQuery_ conn q = liftIO $ do
    formatQuery conn q () >>= print
    query_ conn q

myExecute :: (ToRow q, MonadIO m) => Connection -> Query -> q -> m Int64
myExecute conn q params = liftIO $ do
    formatQuery conn q params >>= print
    execute conn q params

myExecuteMany :: (ToRow q, MonadIO m) => Connection -> Query -> [q] -> m Int64
myExecuteMany conn q params = liftIO $ do
    formatMany conn q params >>= print
    executeMany conn q params

withTransaction :: (MonadIO m) => Connection -> IO a -> m a
withTransaction conn action = liftIO $ PSQL.withTransaction conn action
