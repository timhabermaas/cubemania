{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Workers
    ( wastedTimeThread
    , emailWorkerThread
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
import Mailer

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
                      if submittedUserName == "blub3" then
                          fail "foo"
                      else
                          pure $ (flip runReaderT) config $ sendMail $ registerMail submittedUserEmail submittedUserName
                  _ -> pure $ (pure () :: IO ())
        action

wastedTimeThread :: TChan Event -> WastedTimeStore -> IO ()
wastedTimeThread channel store = forever $ do
    atomically $ do
        event <- readTChan channel
        case event of
            SingleSubmitted userId single ->
                addWastedTime store userId (submittedSingleTime single)
            SingleDeleted single ->
                removeWastedTime store (singleUserId single) (singleTime single)
            _ -> pure ()
