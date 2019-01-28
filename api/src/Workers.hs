{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Workers
    ( wastedTimeThread
    , emailWorkerThread
    , recordWorkerThread
    ) where

import Types
import Types.Stores
import Types.Events
import Types.Configuration
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import Control.Monad (forever)
import Control.Monad.Reader
import Control.Exception (catch, SomeException)
import qualified Db
import Mailer
import qualified RecordCalculation as RC

emailWorkerThread :: TChan Event -> Configuration -> IO ()
emailWorkerThread channel config = do
    catch job (\e -> putStrLn $ "can't send email because of: " ++ show (e :: SomeException))
    emailWorkerThread channel config
  where
    job = do
        action <- atomically $ do
            event <- readTChan channel
            case event of
                UserRegistered SubmittedUser{..} -> do
                    pure $ (flip runReaderT) config $ sendMail $ registerMail submittedUserEmail submittedUserName
                UserPasswordReseted email name password -> do
                    pure $ (flip runReaderT) config $ sendMail $ resetPasswordMail email name password
                _ -> pure $ (pure () :: IO ())

        action

wastedTimeThread :: TChan Event -> WastedTimeStore -> IO ()
wastedTimeThread channel store = forever $ do
    atomically $ do
        event <- readTChan channel
        case event of
            SingleSubmitted userId _ single ->
                addWastedTime store userId (submittedSingleTime single)
            SingleDeleted single ->
                removeWastedTime store (singleUserId single) (singleTime single)
            _ -> pure ()

recordWorkerThread :: TChan Event -> Configuration -> IO ()
recordWorkerThread channel Configuration{..} = forever $ do
    join $ atomically $ do
        event <- readTChan channel
        case event of
            SingleSubmitted userId puzzleId _ ->
                -- Returning an IO action because we are operating within STM where we don't have access to IO.
                pure $ do
                    forM_ allRecordTypes $ \recordType -> do
                        -- TODO: Make more resiliant, worker shouldn't crash when DB connection is lost.
                        recentSingles <- Db.withPool getPool (\c -> Db.getSingles puzzleId userId (Limit $ singleCount recordType) c)
                        currentRecord <- Db.withPool getPool $ Db.getRecordForUserAndPuzzleAndType userId puzzleId recordType
                        let newBestTime = RC.newRecord recentSingles recordType (recordTime <$> dbEntryRow <$> currentRecord)
                        case newBestTime of
                            Just time -> do
                                let setAt = singleCreatedAt $ head recentSingles
                                let newRecord = Record { recordTime = time, recordComment = "", recordPuzzleId = puzzleId, recordUserId = userId, recordType = recordType, recordSetAt = setAt }
                                -- Partial function is safe: We can't get a new record with zero singles
                                Db.withPool getPool $ Db.saveRecord newRecord (singleId <$> recentSingles)
                                pure ()
                            Nothing ->
                                pure ()
            SingleDeleted s ->
                pure $ do
                    let userId = singleUserId s
                    let puzzleId = singlePuzzleId s
                    Db.withPool getPool $ Db.deleteAllRecordsForUserAndPuzzle userId puzzleId
                    allSingles <- Db.withPool getPool $ Db.getSingles puzzleId userId NoLimit
                    -- TODO: Avoid iterating through singles three times by
                    --       passing `Map RecordType (Maybe (DurationInMs, [Single])) around
                    --       Alternatively: Create a thread per record type.
                    forM_ allRecordTypes $ \recordType -> do
                        case RC.calculateRecord allSingles recordType of
                            Just (newRecordTime, singles) -> do
                                let setAt = singleCreatedAt $ head singles
                                let newDBRecord = Record { recordTime = newRecordTime, recordComment = "", recordPuzzleId = puzzleId, recordUserId = userId, recordType = recordType, recordSetAt = setAt }
                                Db.withPool getPool $ Db.saveRecord newDBRecord (singleId <$> singles)
                                pure ()
                            Nothing -> pure ()
                    pure ()
            _ -> pure $ pure ()
