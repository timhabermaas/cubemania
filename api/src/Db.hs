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
    ) where

import Types
import Utils
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime)
import Data.Time.LocalTime (localTimeToUTC, utc)
import Control.Monad.Reader (MonadReader, MonadIO, asks)
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple (Connection, query, query_, execute, withTransaction, Only(..))

import Debug.Trace

runDb :: (MonadReader Configuration m, MonadIO m) => (Connection -> m a) -> m a
runDb query = do
    conn <- asks getPool
    query conn

getSingles :: (MonadIO m) => PuzzleId -> UserId -> Limit -> Connection -> m [Single]
getSingles (PuzzleId pid) (UserId uid) (Limit limit) conn = do
    singles <- liftIO $ query conn "select id, time, comment, scramble, penalty, created_at from singles where puzzle_id = ? and user_id = ? ORDER BY created_at DESC LIMIT ?" (pid, uid, limit)
    return singles

getSingle :: (MonadIO m) => SingleId -> Connection -> m Single
getSingle (SingleId id) conn = do
    result <- liftIO $ query conn "SELECT id, time, comment, scramble, penalty, created_at FROM singles WHERE id = ?" (Only id)
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

postSingle :: (MonadIO m) => PuzzleId -> SubmittedSingle -> Connection -> m SingleId
postSingle (PuzzleId pid) (SubmittedSingle s t) conn = do
    let userId = 3 :: Int
    result :: [Only Int] <- liftIO $ withTransaction conn $ do
        time <- getCurrentTime
        execute conn "UPDATE users SET singles_count = COALESCE(singles_count, 0) + 1 WHERE users.id = ?" (Only userId)
        query conn "INSERT INTO singles (time, puzzle_id, user_id, scramble, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?) RETURNING id" (t, pid, userId, s, time, time)
    case safeHead result of
        Just (Only x) -> return $ SingleId $ x
        Nothing -> error "not gonna happen"
userIdOfSingle :: (MonadIO m) => SingleId -> Connection -> m UserId
userIdOfSingle (SingleId singleId) conn = do
    result <- liftIO $ query conn "SELECT user_id FROM singles WHERE id = ?" (Only singleId)
    return $ head result

deleteSingle :: (MonadIO m) => SingleId -> Connection -> m ()
deleteSingle s@(SingleId singleId) conn = do
    liftIO $ withTransaction conn $ do
        (UserId userId) <- userIdOfSingle s conn
        execute conn "UPDATE users SET singles_count = COALESCE(singles_count, 0) - 1 WHERE users.id = ?" (Only userId)
        execute conn "DELETE FROM singles WHERE id = ?" (Only singleId)
        return ()

matchUsers :: (MonadIO m) => String -> Connection -> m [SimpleUser]
matchUsers q conn = do
    liftIO $ query conn "SELECT id, slug, name, singles_count FROM users WHERE lower(name) LIKE ? LIMIT 200" (Only $ ('%':q) ++ ['%'])

getUsers :: (MonadIO m) => Connection -> m [SimpleUser]
getUsers conn = do
    liftIO $ query conn "SELECT id, slug, name, singles_count FROM users ORDER BY singles_count DESC LIMIT 200" ()

maxSinglesCount :: (MonadIO m) => Connection -> m (Maybe Int)
maxSinglesCount conn = do
    counts <- liftIO $ query_ conn "SELECT singles_count FROM users ORDER BY singles_count DESC LIMIT 1"
    case counts of
      [Only x] -> return $ Just x
      []  -> return $ Nothing

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
