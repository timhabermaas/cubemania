{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}

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
import Data.Maybe (fromMaybe, isJust, fromJust)
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

newtype CubemaniaApp a
  = CubemaniaApp
  { runApp :: ReaderT Config.Configuration (ExceptT ServantErr IO) a
  } deriving (Functor, Applicative, Monad, MonadReader Config.Configuration,
              MonadError ServantErr, MonadIO)

runCubemania :: Config.Configuration -> CubemaniaApp a -> ExceptT ServantErr IO a
runCubemania config (CubemaniaApp app) = runReaderT app config

startApp :: String -> IO ()
startApp dbConnectionString = do
  conn <- connectPostgreSQL $ pack dbConnectionString
  channel <- atomically $ newBroadcastTChan
  wastedTimeStore <- atomically $ newWastedTimeStore
  let c = Config.Configuration conn wastedTimeStore channel
      port = 9090
  putStrLn $ "Starting server on port " ++ show port
  wastedTimeChannel <- atomically $ dupTChan channel
  forkIO $ wastedTimeThread wastedTimeChannel wastedTimeStore
  run port $ logStdoutDev $ app c

convertApp :: Config.Configuration -> CubemaniaApp :~> ExceptT ServantErr IO
convertApp cfg = Nat $ runCubemania cfg

appToServer :: Config.Configuration -> Server CubemaniaAPI
appToServer cfg = enter (convertApp cfg) allHandlers

unauthorized :: MonadError ServantErr m => m a
unauthorized = throwError (err401 { errBody = "Missing auth cookie" })

authHandler :: Config.Configuration -> AuthHandler Request (LoggedIn User)
authHandler configuration =
    mkAuthHandler $ (runCubemania configuration) . handler
  where
    handler req = do
      let userId = parseSessionCookie req
      case userId of
        Just u -> do
          -- TODO: use maybe
          user <- Db.runDb $ Db.getUserById u
          case user of
            Just u -> return $ LoggedIn u
            Nothing -> unauthorized
        Nothing -> unauthorized

parseSessionCookie :: Request -> Maybe UserId
parseSessionCookie req =
     case lookup "Cookie" (requestHeaders req) of
       Just x ->
         case bar2 x >>= listToPair of
           Just (_key, value) ->
             case safeRead $ unpack value of
               Just x' -> Just $ UserId x'
               Nothing -> Nothing
           Nothing -> Nothing
       Nothing -> Nothing
  where
    foo x = split (fromIntegral $ ord ';') x
    bar x = Data.ByteString.filter (\x' -> fromIntegral x' /= ord ' ') <$> foo x
    bar2 x = safeHead $ split (fromIntegral $ ord '=') <$> Prelude.filter (isPrefixOf "current_user_id") (bar x)

    listToPair :: [a] -> Maybe (a, a)
    listToPair [a, b] = Just (a, b)
    listToPair _      = Nothing


authHandlerOptional :: Config.Configuration -> AuthHandler Request (Maybe (LoggedIn User))
authHandlerOptional configuration =
    mkAuthHandler ((runCubemania configuration) . handler)
  where
    handler req = do
        case parseSessionCookie req of
          Just u -> do
            (fmap LoggedIn) <$> (Db.runDb $ Db.getUserById u)
          Nothing -> return Nothing


apiContext :: Config.Configuration -> Context (AuthHandler Request (Maybe (LoggedIn User))  ': AuthHandler Request (LoggedIn User) ': '[])
apiContext config = (authHandlerOptional config) :. (authHandler config)  :. EmptyContext

app :: Config.Configuration -> Application
app config = serveWithContext api (apiContext config) $ appToServer config

allHandlers :: ServerT CubemaniaAPI CubemaniaApp
allHandlers = jsonApiHandler :<|> usersHandler :<|> userHandler :<|> postHandler :<|> rootHandler
  where
    jsonApiHandler = puzzleHandler :<|> usersApiHandler
    puzzleHandler puzzleId = singlesHandler puzzleId
                        :<|> recordsHandler puzzleId
                        :<|> chartHandler puzzleId
                        :<|> protectedHandlers puzzleId
    usersHandler currentUser query page = do
        let pageNumber = fromMaybe (PageNumber 1) page
        users <- case query of
            Just q -> Db.runDb $ Db.matchUsers q
            Nothing -> Db.runDb $ Db.getUsers (fromPageNumber pageNumber)
        maxSinglesCount <- Db.runDb Db.maxSinglesCount
        return $ H.usersPage currentUser users (fromMaybe 1 maxSinglesCount) pageNumber query
    userHandler currentUser userSlug = do
        user <- Db.runDb $ Db.getUserBySlug userSlug
        case user of
            Just u -> do
                records <- Db.runDb $ Db.getRecordsForUser (userId u)
                activity <- Db.runDb $ Db.getActivity (userId u)
                return $ H.userPage currentUser u records activity
            Nothing -> notFound
    postHandler currentUser pId = do
        post <- Db.runDb $ Db.getAnnouncement pId
        case post of
            Just p -> do
              user <- Db.runDb $ Db.getUserById $ announcementUserId p
              comments' <- Db.runDb $ Db.getCommentsForAnnouncement $ announcementId p
              comments <- mapM (\c@Comment{..} -> (if isJust commentAuthorId then Db.runDb $ Db.getUserById (fromJust commentAuthorId) else return Nothing) >>= \u -> return (c, u)) comments'
              return $ H.postPage currentUser p user comments
            Nothing -> notFound

    rootHandler currentUser = do
        announcement <- Db.runDb Db.getLatestAnnouncement
        comments <- maybe (return []) (\a -> Db.runDb $ Db.getCommentsForAnnouncement (announcementId a)) announcement
        return $ H.rootPage currentUser ((\a -> (a, comments)) <$> announcement)
    protectedHandlers puzzleId user = submitSingleHandler puzzleId user
                                 :<|> deleteSingleHandler puzzleId user
                                 :<|> updateSingleHandler puzzleId user



recordsHandler :: PuzzleId -> Maybe Int -> Maybe UserId -> CubemaniaApp [Record]
recordsHandler puzzleId _page userId =
    case userId of
        Nothing -> return []
        Just uid -> Db.runDb $ Db.getRecords uid puzzleId

singlesHandler :: PuzzleId -> Maybe UserId -> Maybe Limit -> CubemaniaApp [Single]
singlesHandler puzzleId userId limit = do
    case userId of
        Nothing -> return []
        Just uid -> Db.runDb $ Db.getSingles puzzleId uid $ fromMaybe (Limit 150) limit

submitSingleHandler :: PuzzleId -> LoggedIn User -> SubmittedSingle -> CubemaniaApp (Headers '[Header "X-NewRecord" String] Single)
submitSingleHandler p (LoggedIn user) s = do
    -- TODO: DB transaction
    singleId' <- Db.runDb $ Db.postSingle p (userId user) s
    single <- Db.runDb $ Db.getSingle singleId'
    publishEvent (SingleSubmitted (userId user) s)
    return $ addHeader "true" single

updateSingleHandler :: PuzzleId -> LoggedIn User -> SingleId -> SubmittedSingle -> CubemaniaApp NoContent
updateSingleHandler _puzzleId (LoggedIn user) singleId' s = do
    single <- Db.runDb $ Db.getSingle singleId'
    if singleUserId single == userId user then
      do
        Db.runDb $ Db.updateSingle singleId' s
        return NoContent
    else
      unauthorized

deleteSingleHandler :: PuzzleId -> LoggedIn User -> SingleId -> CubemaniaApp NoContent
deleteSingleHandler _puzzleId (LoggedIn user) singleId' = do
    single <- Db.runDb $ Db.getSingle singleId'
    if (singleUserId single) == (userId user) then
      do
        Db.runDb $ Db.deleteSingle singleId'
        publishEvent (SingleDeleted single)
        return NoContent
    else
      unauthorized

chartHandler :: PuzzleId -> Maybe Float -> Maybe Float -> Maybe UserId -> CubemaniaApp [ChartData]
chartHandler puzzleId from to userId = do
    case userId of
      Just uid -> Db.runDb $ Db.getChartData puzzleId uid (epochToUTCTime <$> from, epochToUTCTime <$> to)
      Nothing -> return []
  where
    epochToUTCTime :: Float -> UTCTime
    epochToUTCTime = posixSecondsToUTCTime . (fromIntegral :: Int -> NominalDiffTime) . floor

usersApiHandler :: Maybe T.Text -> CubemaniaApp [SimpleUser]
usersApiHandler q = do
    case q of
        Just query -> Db.runDb $ Db.matchUsers query
        Nothing    -> return []

notFound :: MonadError ServantErr m => m a
notFound = throwError $ err404 { errBody = "(╯°□°）╯︵ ┻━┻).", errHeaders = [("Content-Type", "text/html; charset=UTF-8")] }

publishEvent :: (MonadReader Config.Configuration m, MonadIO m) => Event -> m ()
publishEvent event = do
    chan <- asks Config.eventChannel
    liftIO $ atomically $ writeTChan chan event
