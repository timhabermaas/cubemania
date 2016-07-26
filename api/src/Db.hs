{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}

module Db
    ( matchUsers
    , getRecords
    , getSingles
    , runDb
    , getChartData
    ) where

import Types
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime)
import Data.Time.LocalTime (localTimeToUTC, utc)
import Control.Monad.Reader (MonadReader, MonadIO, asks)
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple (Connection, query, Only(..))

import Debug.Trace

runDb :: (MonadReader Configuration m, MonadIO m) => (Connection -> m a) -> m a
runDb query = do
    conn <- asks getPool
    query conn

getSingles :: (MonadIO m) => PuzzleId -> UserId -> Limit -> Connection -> m [Single]
getSingles (PuzzleId pid) (UserId uid) (Limit limit) conn = do
    singles <- liftIO $ query conn "select id, time, comment, scramble, penalty, created_at from singles where puzzle_id = ? and user_id = ? ORDER BY created_at DESC LIMIT ?" (pid, uid, limit)
    return singles

getRecords :: (MonadIO m) => UserId -> PuzzleId -> Connection -> m [Record]
getRecords (UserId uid) (PuzzleId pid) conn = do
    records <- liftIO $ query conn "select id, time, comment, puzzle_id, amount from records where user_id = ? and puzzle_id = ?" (uid, pid)
    liftIO $ mapM (grabSingles conn) records

grabSingles :: Connection -> Record -> IO Record
grabSingles conn r = do
    singles <- query conn "SELECT singles.id, singles.time, singles.scramble FROM singles INNER JOIN records_singles ON singles.id = records_singles.single_id WHERE records_singles.record_id = ?" (Only $ recordId r)
    return $ r { recordSingles = singles }

matchUsers :: (MonadIO m) => String -> Connection -> m [SimpleUser]
matchUsers q conn = do
    liftIO $ query conn "SELECT id, slug, name, singles_count FROM users WHERE lower(name) LIKE ? LIMIT 200" (Only $ ('%':q) ++ ['%'])

getChartData :: (MonadIO m) => PuzzleId -> UserId -> (Maybe UTCTime, Maybe UTCTime) -> Connection -> m [ChartData]
getChartData (PuzzleId puzzleId) (UserId userId) (from, to) conn = do
    liftIO $ putStrLn $ show (from, to)
    min <- case from of
             Just t -> return t
             Nothing -> liftIO minimumSingle
    max <- case to of
             Just t -> return t
             Nothing -> liftIO getCurrentTime
    liftIO $ putStrLn $ show (min, max)
    let difference = diffUTCTime max min
    liftIO $ putStrLn $ show difference
    case groupingForDateDiff difference of
        Just group -> liftIO $ groupSingles (min, max) group
        Nothing -> liftIO $ fetchSingles (min, max)
  where
    groupingForDateDiff :: NominalDiffTime -> Maybe String
    groupingForDateDiff diff =
        let
            diffInDays = trace ("foo: " ++ (show $ (toRational diff))) ((toRational diff) / 60.0 / 60.0 / 24.0)
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
