{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Db
    ( matchUsers
    , getRecords
    , getSingles
    , getSingle
    , runDb
    , getChartData
    , getUsers
    , maxSinglesCount
    , postSingle
    , deleteSingle
    , updateSingle
    , getLatestAnnouncement
    ) where

import Types
import Utils
import Data.Monoid ((<>))
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple (Connection, query, query_, execute, withTransaction, Only(..))
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
    records <- liftIO $ query conn "select id, time, comment, puzzle_id, amount from records where user_id = ? and puzzle_id = ?" (uid, pid)
    liftIO $ mapM (grabSingles conn) records

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

updateSingle :: (MonadIO m) => PuzzleId -> SingleId -> SubmittedSingle -> Connection -> m SingleId
updateSingle (PuzzleId pid) (SingleId sid) s conn = do
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

getUsers :: (MonadIO m) => Int -> Connection -> m [SimpleUser]
getUsers page conn = do
    let offset = (page - 1) * 200
    liftIO $ query conn "SELECT id, slug, name, singles_count FROM users ORDER BY singles_count DESC LIMIT 200 OFFSET ?" (Only offset)

maxSinglesCount :: (MonadIO m) => Connection -> m (Maybe Int)
maxSinglesCount conn = do
    counts <- liftIO $ query_ conn "SELECT singles_count FROM users ORDER BY singles_count DESC LIMIT 1"
    case counts of
      [Only x] -> return $ Just x
      _        -> return $ Nothing

getChartData :: (MonadIO m) => PuzzleId -> UserId -> (Maybe UTCTime, Maybe UTCTime) -> Connection -> m [ChartData]
getChartData (PuzzleId puzzleId) (UserId userId) (from, to) conn = do
    min <- case from of
             Just t -> return t
             Nothing -> liftIO minimumSingle
    max <- case to of
             Just t -> return t
             Nothing -> liftIO getCurrentTime
    let difference = diffUTCTime max min
    case groupingForDateDiff difference of
        Just group -> liftIO $ groupSingles (min, max) group
        Nothing -> liftIO $ fetchSingles (min, max)
  where
    groupingForDateDiff :: NominalDiffTime -> Maybe String
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
    groupSingles :: (UTCTime, UTCTime) -> String -> IO [ChartData]
    groupSingles (from, to) grouping = query conn "SELECT AVG(time) as time, string_agg(comment, '\n') AS comment, date_trunc(?, created_at) as created_at FROM singles WHERE (penalty IS NULL OR penalty NOT LIKE 'dnf') AND puzzle_id = ? AND user_id = ? AND created_at BETWEEN ? AND ? GROUP BY date_trunc(?, created_at) ORDER BY created_at" (grouping, puzzleId, userId, from, to, grouping)
    fetchSingles :: (UTCTime, UTCTime) -> IO [ChartData]
    fetchSingles (from, to) = query conn "SELECT time, comment, created_at FROM singles WHERE (penalty IS NULL OR penalty NOT LIKE 'dnf') AND puzzle_id = ? AND user_id = ? AND created_at BETWEEN ? AND ? ORDER BY created_at" (puzzleId, userId, from, to)
    minimumSingle :: IO UTCTime
    minimumSingle = (localTimeToUTCTime . head) <$> query conn "SELECT created_at FROM singles WHERE puzzle_id = ? AND user_id = ? ORDER BY created_at LIMIT 1" (puzzleId, userId)

getLatestAnnouncement :: (MonadIO m) => Connection -> m (Maybe Announcement)
getLatestAnnouncement conn = do
    posts <- liftIO $ query_ conn "SELECT id, title, content, user_id FROM posts ORDER BY created_at desc LIMIT 1"
    return $ safeHead posts
