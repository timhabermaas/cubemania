{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Lib
    ( startApp
    ) where

import qualified Db as Db
import Utils

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)

import Data.ByteString (split, filter, isPrefixOf)
import Data.ByteString.Char8 (unpack)
import Data.Char (ord)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromMaybe)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (newBroadcastTChan, dupTChan, writeTChan)

import Types
import qualified Types.Configuration as Config
import Types.Stores (newWastedTimeStore)
import Types.Events
import Routes
import qualified Html as H
import Workers (wastedTimeThread)

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Except
import Database.PostgreSQL.Simple (connectPostgreSQL)

newtype MyStack a
  = MyStack
  { runApp :: ReaderT Config.Configuration (ExceptT ServantErr IO) a
  } deriving (Functor, Applicative, Monad, MonadReader Config.Configuration,
              MonadError ServantErr, MonadIO)

startApp :: String -> IO ()
startApp dbConnectionString = do
  conn <- connectPostgreSQL $ pack dbConnectionString
  channel <- atomically $ newBroadcastTChan
  wastedTimeStore <- atomically $ newWastedTimeStore
  let c = Config.Configuration conn wastedTimeStore channel
      port = 9090
  putStrLn $ "Starting server on port " ++ show port
  channelForWastedTimeThread <- atomically $ dupTChan channel
  forkIO $ wastedTimeThread channelForWastedTimeThread wastedTimeStore
  run port $ logStdoutDev $ app c

convertApp :: Config.Configuration -> MyStack :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

appToServer :: Config.Configuration -> Server CubemaniaAPI
appToServer cfg = enter (convertApp cfg) allHandlers

unauthorized :: MonadError ServantErr m => m a
unauthorized = throwError (err401 { errBody = "Missing auth cookie" })

authHandler :: AuthHandler Request UserId
authHandler =
    mkAuthHandler handler
  where
    handler req =
      case lookup "Cookie" (requestHeaders req) of
        Just x -> do
          let foo = split (fromIntegral $ ord ';') x
          let bar = Data.ByteString.filter (\x' -> fromIntegral x' /= ord ' ') <$> foo
          let bar2 = safeHead $ split (fromIntegral $ ord '=') <$> Prelude.filter (isPrefixOf "current_user_id") bar
          case bar2 >>= listToPair of
            Just (_key, value) ->
              case safeRead $ unpack value of
                Just x' -> return $ UserId x'
                Nothing -> unauthorized
            Nothing -> unauthorized
        Nothing -> unauthorized

    listToPair :: [a] -> Maybe (a, a)
    listToPair [a, b] = Just (a, b)
    listToPair _      = Nothing

apiContext :: Context (AuthHandler Request UserId ': '[])
apiContext = authHandler :. EmptyContext

app :: Config.Configuration -> Application
app config = serveWithContext api apiContext $ appToServer config

allHandlers :: ServerT CubemaniaAPI MyStack
allHandlers = puzzleHandler :<|> usersApiHandler :<|> usersHandler :<|> userHandler :<|> rootHandler
  where
    puzzleHandler puzzleId = singlesHandler puzzleId
                        :<|> recordsHandler puzzleId
                        :<|> chartHandler puzzleId
                        :<|> protectedHandlers puzzleId
    usersHandler query page = do
        let pageNumber = fromMaybe (PageNumber 1) page
        users <- case query of
            Just q -> Db.runDb $ Db.matchUsers q
            Nothing -> Db.runDb $ Db.getUsers (fromPageNumber pageNumber)
        maxSinglesCount <- Db.runDb Db.maxSinglesCount
        return $ H.usersPage users (fromMaybe 1 maxSinglesCount) pageNumber query
    userHandler userSlugOrId = do
        user <- Db.runDb $ Db.getUser userSlugOrId
        case user of
            Just u -> return $ H.userPage u
            Nothing -> notFound

    rootHandler = do
        announcement <- Db.runDb Db.getLatestAnnouncement
        return $ H.rootPage announcement
    protectedHandlers puzzleId userId = submitSingleHandler puzzleId userId
                                   :<|> deleteSingleHandler puzzleId userId
                                   :<|> updateSingleHandler puzzleId userId



recordsHandler :: PuzzleId -> Maybe Int -> Maybe UserId -> MyStack [Record]
recordsHandler puzzleId _page userId =
    case userId of
        Nothing -> return []
        Just uid -> Db.runDb $ Db.getRecords uid puzzleId

singlesHandler :: PuzzleId -> Maybe UserId -> Maybe Limit -> MyStack [Single]
singlesHandler puzzleId userId limit = do
    case userId of
        Nothing -> return []
        Just uid -> Db.runDb $ Db.getSingles puzzleId uid $ fromMaybe (Limit 150) limit

submitSingleHandler :: PuzzleId -> UserId -> SubmittedSingle -> MyStack (Headers '[Header "X-NewRecord" String] Single)
submitSingleHandler p userId s = do
    singleId' <- Db.runDb $ Db.postSingle p userId s
    single <- Db.runDb $ Db.getSingle singleId'
    publishEvent (SingleSubmitted userId s)
    --liftIO $ atomically $ writeTChan chan (SingleSubmitted userId (submittedSingleTime s))
    return $ addHeader "true" single

updateSingleHandler :: PuzzleId -> UserId -> SingleId -> SubmittedSingle -> MyStack NoContent
updateSingleHandler _puzzleId userId singleId' s = do
    single <- Db.runDb $ Db.getSingle singleId'
    if (singleUserId single) == userId then
      do
        Db.runDb $ Db.updateSingle singleId' s
        return NoContent
    else
      unauthorized

deleteSingleHandler :: PuzzleId -> UserId -> SingleId -> MyStack NoContent
deleteSingleHandler _puzzleId userId singleId' = do
    single <- Db.runDb $ Db.getSingle singleId'
    if (singleUserId single) == userId then
      do
        Db.runDb $ Db.deleteSingle singleId'
        publishEvent (SingleDeleted single)
        return NoContent
    else
      unauthorized

chartHandler :: PuzzleId -> Maybe Float -> Maybe Float -> Maybe UserId -> MyStack [ChartData]
chartHandler puzzleId from to userId = do
    case userId of
      Just uid -> Db.runDb $ Db.getChartData puzzleId uid (epochToUTCTime <$> from, epochToUTCTime <$> to)
      Nothing -> return []
  where
    epochToUTCTime :: Float -> UTCTime
    epochToUTCTime = posixSecondsToUTCTime . (fromIntegral :: Int -> NominalDiffTime) . floor

usersApiHandler :: Maybe T.Text -> MyStack [SimpleUser]
usersApiHandler q = do
    case q of
        Just query -> Db.runDb $ Db.matchUsers query
        Nothing    -> return []

notFound :: MonadError ServantErr m => m a
notFound = throwError $ err404 { errBody = "(╯°□°）╯︵ ┻━┻).", errHeaders = [("Content-Type", "text/html; charset=UTF-8")] }

publishEvent :: (MonadReader Config.Configuration m, MonadIO m) => Event -> m Event
publishEvent event = do
    chan <- asks Config.eventChannel
    liftIO $ atomically $ writeTChan chan event
    return event
