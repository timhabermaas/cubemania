{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE BangPatterns               #-}

module Db
    ( matchUsers
    , getRecords
    , getRecordsForPuzzleAndType
    , getRecordCountForPuzzleAndType
    , getSingles
    , getSingle
    , runDb
    , getChartData
    , getUsers
    , getUserBySlug
    , getUserById
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
    , getActivity
    , getAllSingles
    , getPuzzleBySlug
    , getKindById
    , getAllPuzzles
    ) where

import Prelude hiding (id)
import Types
import Utils
import Types.Configuration
import Data.Monoid ((<>))
import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime, utctDay)
import Data.Time.Calendar (addDays)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple (Connection, query, query_, fold_, execute, withTransaction, Only(..))
import Data.Text (Text)

runDb :: (MonadReader Configuration m, MonadIO m) => (Connection -> m a) -> m a
runDb q = do
    conn <- asks getPool
    q conn

getSingles :: (MonadIO m) => PuzzleId -> UserId -> Limit -> Connection -> m [Single]
getSingles (PuzzleId pid) (UserId uid) (Limit limit) conn = do
    singles <- liftIO $ query conn "select id, time, comment, scramble, penalty, created_at, user_id from singles where puzzle_id = ? and user_id = ? ORDER BY created_at DESC LIMIT ?" (pid, uid, limit)
    return singles

getSingle :: (MonadIO m) => SingleId -> Connection -> m Single
getSingle (SingleId id) conn = do
    result <- liftIO $ query conn "SELECT id, time, comment, scramble, penalty, created_at, user_id FROM singles WHERE id = ?" (Only id)
    case safeHead result of
      Just x -> return x
      Nothing -> error "nope"


getRecords :: (MonadIO m) => UserId -> PuzzleId -> Connection -> m [Record]
getRecords (UserId uid) (PuzzleId pid) conn = do
    records <- liftIO $ query conn "select id, time, comment, puzzle_id, user_id, amount, updated_at from records where user_id = ? and puzzle_id = ?" (uid, pid)
    liftIO $ mapM (grabSingles conn) records

getRecordsForPuzzleAndType :: (MonadIO m) => PuzzleId -> RecordType -> Int -> Connection -> m [(Record, SimpleUser)]
getRecordsForPuzzleAndType pId type' page conn = do
    let offset = (page - 1) * 50
    foo <- liftIO $ query conn "SELECT records.id, records.time, records.comment, records.puzzle_id, records.user_id, records.amount, records.updated_at, users.id, users.slug, users.name, users.singles_count FROM records LEFT OUTER JOIN users ON users.id = records.user_id WHERE records.puzzle_id = ? AND records.amount = ? AND users.ignored = 'f' ORDER BY records.time OFFSET ? LIMIT 50" (pId, type', offset)
    return $ unwrapJoinedResult2 <$> foo

getRecordCountForPuzzleAndType :: (MonadIO m) => PuzzleId -> RecordType -> Connection -> m Int
getRecordCountForPuzzleAndType pId type' conn = do
    [Only i] <- liftIO $ query conn "SELECT COUNT(DISTINCT records.id) FROM records LEFT OUTER JOIN users ON users.id = records.user_id WHERE records.puzzle_id = ? AND users.ignored = 'f' AND records.amount = ?" (pId, type')
    return i


grabSingles :: Connection -> Record -> IO Record
grabSingles conn r = do
    singles <- query conn "SELECT singles.id, singles.time, singles.scramble FROM singles INNER JOIN records_singles ON singles.id = records_singles.single_id WHERE records_singles.record_id = ?" (Only $ recordId r)
    return $ r { recordSingles = singles }

postSingle :: (MonadIO m) => PuzzleId -> UserId -> SubmittedSingle -> Connection -> m SingleId
postSingle (PuzzleId pid) (UserId userId) (SubmittedSingle s t _p) conn = do
    result :: [Only Int] <- liftIO $ withTransaction conn $ do
        time <- getCurrentTime
        _ <- execute conn "UPDATE users SET singles_count = COALESCE(singles_count, 0) + 1 WHERE users.id = ?" (Only userId)
        query conn "INSERT INTO singles (time, puzzle_id, user_id, scramble, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?) RETURNING id" (t, pid, userId, s, time, time)
    case safeHead result of
        Just (Only x) -> return $ SingleId $ x
        Nothing -> error "not gonna happen"

updateSingle :: (MonadIO m) => SingleId -> SubmittedSingle -> Connection -> m SingleId
updateSingle (SingleId sid) s conn = do
    time' <- liftIO getCurrentTime
    liftIO $ execute conn "UPDATE singles SET time=?, updated_at=?, penalty=? WHERE id = ?" (submittedSingleTime s, time', submittedSinglePenalty s, sid)
    return $ SingleId sid

deleteSingle :: (MonadIO m) => SingleId -> Connection -> m ()
deleteSingle s@(SingleId singleId) conn = do
    liftIO $ withTransaction conn $ do
        (UserId userId) <- singleUserId <$> getSingle s conn
        _ <- execute conn "UPDATE users SET singles_count = COALESCE(singles_count, 0) - 1 WHERE users.id = ?" (Only userId)
        _ <- execute conn "DELETE FROM singles WHERE id = ?" (Only singleId)
        return ()

matchUsers :: (MonadIO m) => Text -> Connection -> m [SimpleUser]
matchUsers q conn =
    liftIO $ query conn "SELECT id, slug, name, singles_count FROM users WHERE lower(name) LIKE ? ORDER BY singles_count DESC LIMIT 200" (Only $ "%" <> q <> "%")

getUserBySlug :: (MonadIO m) => UserSlug -> Connection -> m (Maybe User)
getUserBySlug slug conn = do
    users <- liftIO $ query conn "SELECT id, name, slug, email, role, wca, ignored, wasted_time FROM users WHERE slug = ?" (Only slug)
    return $ safeHead users

getUserById :: (MonadIO m) => UserId -> Connection -> m (Maybe User)
getUserById id conn = do
    users <- liftIO $ query conn "SELECT id, name, slug, email, role, wca, ignored, wasted_time FROM users WHERE id = ?" (Only id)
    return $ safeHead users

getAnnouncement :: (MonadIO m) => AnnouncementId -> Connection -> m (Maybe Announcement)
getAnnouncement id conn = do
    posts <- liftIO $ query conn "SELECT id, title, content, user_id, created_at FROM posts WHERE id = ?" (Only id)
    return $ safeHead posts

getAnnouncements :: (MonadIO m) => Connection -> m [Announcement]
getAnnouncements conn =
    liftIO $ query_ conn "SELECT id, title, content, user_id, created_at FROM posts ORDER BY created_at DESC"

getCommentsForAnnouncement :: (MonadIO m) => AnnouncementId -> Connection -> m [Comment]
getCommentsForAnnouncement id conn = do
    liftIO $ query conn "SELECT id, content, user_id, created_at FROM comments WHERE commentable_id = ? AND commentable_type = 'Post' ORDER BY created_at" (Only id)

data JoinedResult2 a b = JoinedResult2 { unwrapJoinedResult2 :: (a, b) }
instance (FromRow a, FromRow b) => FromRow (JoinedResult2 a b) where
    fromRow = JoinedResult2 <$> ((,) <$> fromRow <*> fromRow)
data JoinedRecordResult = JoinedRecordResult { unwrapJoinedRecordResult :: (Puzzle, Kind, Record) }

instance FromRow JoinedRecordResult where
    fromRow = JoinedRecordResult <$> ((,,) <$> fromRow <*> fromRow <*> fromRow)

getRecordsForUser :: (MonadIO m) => UserId -> Connection -> m (Map.Map (Puzzle, Kind) (Map.Map RecordType DurationInMs))
getRecordsForUser uid conn = do
    result <- liftIO $ query conn "SELECT puzzles.id, puzzles.name, puzzles.css_position, puzzles.slug, puzzles.kind_id, kinds.id, kinds.name, kinds.short_name, kinds.css_position, records.id, records.time, records.comment, records.puzzle_id, records.user_id, records.amount, records.updated_at FROM records LEFT OUTER JOIN puzzles ON puzzles.id = records.puzzle_id LEFT OUTER JOIN kinds ON kinds.id = puzzles.kind_id WHERE records.user_id = ? ORDER BY puzzles.name, kinds.name" (Only uid)
    return $ groupByPuzzle (unwrapJoinedRecordResult <$> result)
  where
    groupByPuzzle :: [(Puzzle, Kind, Record)] -> Map.Map (Puzzle, Kind) (Map.Map RecordType DurationInMs)
    groupByPuzzle i = Map.fromListWith
                        Map.union
                        [((p, k), Map.singleton (recordType r) (recordTime r)) | (p, k, r) <- i]

getUsers :: (MonadIO m) => Int -> Connection -> m [SimpleUser]
getUsers page conn = do
    let offset = (page - 1) * 200
    liftIO $ query conn "SELECT id, slug, name, singles_count FROM users ORDER BY singles_count DESC LIMIT 200 OFFSET ?" (Only offset)

maxSinglesCount :: (MonadIO m) => Connection -> m (Maybe Int)
maxSinglesCount conn = do
    counts <- liftIO $ query_ conn "SELECT singles_count FROM users ORDER BY singles_count DESC LIMIT 1"
    case counts of
      [Only x] -> return $ Just x
      _        -> return Nothing

-- TODO: Make (Maybe UTCTime, Maybe UTCTime) to Maybe (UTCTime, UTCTime) ?
getChartData :: (MonadIO m) => PuzzleId -> UserId -> (Maybe UTCTime, Maybe UTCTime) -> Connection -> m [ChartData]
getChartData (PuzzleId puzzleId) (UserId userId) (from, to) conn = do
    startDate <- case from of
             Just t -> return t
             Nothing -> liftIO firstSingleDate
    endDate <- case to of
             Just t -> return t
             Nothing -> liftIO getCurrentTime
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
    groupSingles (from', to') grouping = query conn "SELECT AVG(time) as time, string_agg(comment, '\n') AS comment, date_trunc(?, created_at) as created_at FROM singles WHERE (penalty IS NULL OR penalty NOT LIKE 'dnf') AND puzzle_id = ? AND user_id = ? AND created_at BETWEEN ? AND ? GROUP BY date_trunc(?, created_at) ORDER BY created_at" (grouping, puzzleId, userId, from', to', grouping)
    fetchSingles :: (UTCTime, UTCTime) -> IO [ChartData]
    fetchSingles (from', to') = query conn "SELECT time, comment, created_at FROM singles WHERE (penalty IS NULL OR penalty NOT LIKE 'dnf') AND puzzle_id = ? AND user_id = ? AND created_at BETWEEN ? AND ? ORDER BY created_at" (puzzleId, userId, from', to')
    firstSingleDate :: IO UTCTime
    firstSingleDate = (localTimeToUTCTime . head) <$> query conn "SELECT created_at FROM singles WHERE puzzle_id = ? AND user_id = ? ORDER BY created_at LIMIT 1" (puzzleId, userId)

getLatestAnnouncement :: (MonadIO m) => Connection -> m (Maybe Announcement)
getLatestAnnouncement conn = do
    posts <- liftIO $ query_ conn "SELECT id, title, content, user_id, created_at FROM posts ORDER BY created_at desc LIMIT 1"
    return $ safeHead posts

postComment :: (MonadIO m) => AnnouncementId -> UserId -> Text -> Connection -> m ()
postComment pId uId content conn = do
    time <- liftIO getCurrentTime
    _ <- liftIO $ execute conn "INSERT INTO comments (content, user_id, commentable_id, commentable_type, created_at) VALUES (?, ?, ?, 'Post', ?)" (content, uId, pId, time)
    return ()

getActivity :: (MonadIO m) => UserId -> Connection -> m (Activity)
getActivity uid conn = do
    today <- liftIO $ utctDay <$> getCurrentTime
    let oneYearAgo = addDays (-366) today
    result <- liftIO $ query conn "SELECT date_trunc('day', created_at), COUNT(*) FROM singles WHERE user_id = ? AND (created_at BETWEEN ? AND ?) GROUP BY date_trunc('day', created_at)" (uid, oneYearAgo, today)
    return $ Activity $ Map.fromList $ fmap (\(t, x) -> (t, x)) result

getAllSingles :: (MonadIO m) => (Single -> IO ()) -> Connection -> m ()
getAllSingles callback conn =
    liftIO $ withTransaction conn $ do
        liftIO $ fold_ conn "select id, time, comment, scramble, penalty, created_at, user_id from singles ORDER BY created_at" () (\() !single -> callback single)

getPuzzleBySlug :: (MonadIO m) => PuzzleSlug -> Connection -> m (Maybe Puzzle)
getPuzzleBySlug pSlug conn = do
    puzzles <- liftIO $ query conn "SELECT id, name, css_position, slug, kind_id FROM puzzles WHERE slug = ?" (Only pSlug)
    pure $ safeHead puzzles

getKindById :: (MonadIO m) => KindId -> Connection -> m (Maybe Kind)
getKindById kId conn = do
    kinds <- liftIO $ query conn "SELECT id, name, short_name, css_position FROM kinds WHERE id = ?" (Only kId)
    pure $ safeHead kinds

getAllPuzzles :: (MonadIO m) => Connection -> m [(Kind, Puzzle)]
getAllPuzzles conn = do
    foo <- liftIO $ query_ conn "SELECT kinds.id, kinds.name, kinds.short_name, kinds.css_position, puzzles.id, puzzles.name, puzzles.css_position, puzzles.slug, puzzles.kind_id FROM puzzles LEFT OUTER JOIN kinds ON kinds.id = puzzles.kind_id ORDER BY puzzles.name"
    return $ unwrapJoinedResult2 <$> foo
