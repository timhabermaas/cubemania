{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Lib
    ( startApp
    ) where

import qualified Db
import Utils

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)

import Data.ByteString (split, filter, isPrefixOf)
import Data.ByteString.Char8 (unpack, pack)
import Data.Char (ord)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Maybe (fromMaybe, isJust, fromJust)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (newBroadcastTChan, dupTChan, writeTChan)

import Types
import qualified Types.Configuration as Config
import Types.Stores (newWastedTimeStore, getWastedTimeFor)
import Types.Events
import Routes
import qualified Html as H
import Workers (wastedTimeThread)
import Frontend.Forms

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Except
import Database.PostgreSQL.Simple (connectPostgreSQL, close)
import Data.Pool (createPool, withResource)
import Types.AppMonad


startApp :: String -> Maybe String -> IO ()
startApp dbConnectionString facebookAppId = do
  pool <- createPool (connectPostgreSQL $ pack dbConnectionString) close 1 10 10
  channel <- atomically newBroadcastTChan
  wastedTimeStore <- atomically newWastedTimeStore
  let c = Config.Configuration pool wastedTimeStore channel facebookAppId
      port = 9090
  putStrLn $ "Starting server on port " ++ show port

  wastedTimeChannel <- atomically $ dupTChan channel
  forkIO $ wastedTimeThread wastedTimeChannel wastedTimeStore
  let makeSubmitEventFromSingle s = SingleSubmitted (singleUserId s) (SubmittedSingle (singleScramble s) (singleTime s) (singlePenalty s))
  let importEvents conn = Db.getAllSingles (\single -> atomically $ writeTChan wastedTimeChannel (makeSubmitEventFromSingle single)) conn

  _ <- forkIO (withResource pool importEvents >> putStrLn "finished importing singles")

  run port $ logStdoutDev $ app c

appToServer :: Config.Configuration -> Server CubemaniaAPI
appToServer cfg = enter (convertApp cfg) allHandlers

unauthorized :: MonadError ServantErr m => m a
unauthorized = throwError (err401 { errBody = "Missing auth cookie" })

redirect303 :: MonadError ServantErr m => T.Text -> m a
redirect303 url = throwError $ err303 { errHeaders = [("Location", TE.encodeUtf8 url)] }

authHandler :: Config.Configuration -> AuthHandler Request LoggedInUser
authHandler configuration =
    mkAuthHandler $ (runCubemania configuration) . handler
  where
    handler :: Request -> CubemaniaApp LoggedInUser
    handler req = do
      maybeUser <- authHandlerOptional' req
      maybe unauthorized return maybeUser

parseSessionCookie :: Request -> Maybe UserId
parseSessionCookie req = do
    x <- lookup "Cookie" (requestHeaders req)
    (_key, value) <- bar2 x >>= listToPair
    x' <- safeRead $ unpack value
    pure $ UserId x'
  where
    foo x = split (fromIntegral $ ord ';') x
    bar x = Data.ByteString.filter (\x' -> fromIntegral x' /= ord ' ') <$> foo x
    bar2 x = safeHead $ split (fromIntegral $ ord '=') <$> Prelude.filter (isPrefixOf "current_user_id") (bar x)

    listToPair :: [a] -> Maybe (a, a)
    listToPair [a, b] = Just (a, b)
    listToPair _      = Nothing


authHandlerOptional :: Config.Configuration -> AuthHandler Request (Maybe (LoggedIn User))
authHandlerOptional configuration =
    mkAuthHandler ((runCubemania configuration) . authHandlerOptional')

authHandlerOptional' :: Request -> CubemaniaApp (Maybe LoggedInUser)
authHandlerOptional' req = do
    case parseSessionCookie req of
      Just u -> do
        (fmap LoggedIn) <$> (Db.runDb $ Db.getUserById u)
      Nothing -> return Nothing


apiContext :: Config.Configuration -> Context (AuthHandler Request (Maybe (LoggedIn User))  ': AuthHandler Request (LoggedIn User) ': '[])
apiContext config = (authHandlerOptional config) :. (authHandler config)  :. EmptyContext

app :: Config.Configuration -> Application
app config = serveWithContext api (apiContext config) $ appToServer config

allHandlers :: ServerT CubemaniaAPI CubemaniaApp
allHandlers = jsonApiHandler :<|> usersHandler :<|> userHandler :<|> postsHandler :<|> postHandler :<|> newPostHandler :<|> createPostHandler :<|> editPostHandler :<|> updatePostHandler :<|> postCommentHandler :<|> recordsHandler :<|> recordHandler :<|> shareRecordHandler :<|> timerHandler :<|> rootHandler
  where
    jsonApiHandler = puzzleHandler :<|> usersApiHandler
    puzzleHandler puzzleId = singlesHandler puzzleId
                        :<|> recordsApiHandler puzzleId
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
        let maybeUser (Just (LoggedIn u)) = Just u
            maybeUser Nothing = Nothing
        user <- grabOrNotFound $ Db.runDb $ Db.getUserBySlug userSlug
        records <- Db.runDb $ Db.getRecordsForUser (userId user)
        let u' = maybeUser currentUser
        ownRecords <- if isJust currentUser && u' /= Just user then
                          Just <$> (Db.runDb $ Db.getRecordsForUser (userId (fromJust u')))
                      else
                          pure Nothing
        activity <- Db.runDb $ Db.getActivity (userId user)
        store <- asks Config.wastedTimeStore
        wastedTime <- fromMaybe 0 <$> (liftIO $ atomically $ getWastedTimeFor store (userId user))
        return $ H.userPage currentUser user records ownRecords activity wastedTime
    postsHandler currentUser = do
        posts <- Db.runDb $ Db.getAnnouncements
        foo <- mapM (\p -> do
                      author <- Db.runDb $ Db.getUserById $ announcementUserId p
                      commentsCount <- length <$> Db.runDb (Db.getCommentsForAnnouncement $ announcementId p)
                      pure (p, author, commentsCount)) posts
        pure $ H.postsPage currentUser foo
    newPostHandler currentUser = do
        form <- runGetForm "post" postForm
        return $ H.newPostPage currentUser form
    createPostHandler cu@(LoggedIn currentUser) body = do
        form <- runPostForm "post" postForm body
        case form of
            (_, Just p) -> do
                pId <- Db.runDb $ Db.postAnnouncement (userId currentUser) p
                redirect303 $ postLinkToComments pId
            (view, Nothing) ->
                pure $ H.newPostPage cu view
    editPostHandler currentUser aId = do
        Announcement{..} <- grabOrNotFound $ Db.runDb $ Db.getAnnouncement aId
        (view, _) <- runPostForm "post" postForm [("post.title", announcementTitle),
                                                  ("post.content", announcementContent)]
        pure $ H.editPostPage currentUser aId view
    updatePostHandler currentUser aId body = do
        Announcement{..} <- grabOrNotFound $ Db.runDb $ Db.getAnnouncement aId
        form <- runPostForm "post" postForm body
        case form of
            (_, Just c) -> do
                Db.runDb $ Db.updateAnnouncement announcementId c
                redirect303 $ postLink announcementId
            (view, Nothing) -> do
                pure $ H.editPostPage currentUser announcementId view
    postHandler currentUser pId = do
        form <- runGetForm "comment" commentForm
        post <- grabOrNotFound $ Db.runDb $ Db.getAnnouncement pId
        renderPostPage currentUser post form
    renderPostPage currentUser post form = do
        user <- Db.runDb $ Db.getUserById $ announcementUserId post
        comments' <- Db.runDb $ Db.getCommentsForAnnouncement $ announcementId post
        comments <- mapM (\c@Comment{..} -> (if isJust commentAuthorId then Db.runDb $ Db.getUserById (fromJust commentAuthorId) else return Nothing) >>= \u -> return (c, u)) comments'
        return $ H.postPage currentUser post user comments form
    postCommentHandler lu@(LoggedIn currentUser) pId body = do
        form <- runPostForm "comment" commentForm body
        post <- grabOrNotFound $ Db.runDb $ Db.getAnnouncement pId
        case form of
            (_, Just c) -> do
                Db.runDb $ Db.postComment (announcementId post) (userId currentUser) (submittedCommentContent c)
                redirect303 $ postLinkToComments (announcementId post)
            (view, Nothing) ->
                renderPostPage (Just lu) post view

    rootHandler currentUser = do
        announcement <- Db.runDb Db.getLatestAnnouncement
        comments <- maybe (return []) (\a -> Db.runDb $ Db.getCommentsForAnnouncement (announcementId a)) announcement
        return $ H.rootPage currentUser ((\a -> (a, comments)) <$> announcement)
    recordsHandler currentUser slug type' page = do
        puzzle <- grabOrNotFound $ Db.runDb $ Db.getPuzzleBySlug slug
        -- TODO: join with puzzle fetch
        kind <- grabOrNotFound $ Db.runDb $ Db.getKindById $ puzzleKindId puzzle
        let pageAsNumber = fromMaybe 1 (fromPageNumber <$> page)
        let recordType = fromMaybe AverageOf5Record type'
        records <- Db.runDb $ Db.getRecordsForPuzzleAndType (puzzleId puzzle) recordType pageAsNumber
        recordsCount <- Db.runDb $ Db.getRecordCountForPuzzleAndType (puzzleId puzzle) recordType
        allPuzzles <- Db.runDb Db.getAllPuzzles
        pure $ H.recordsPage currentUser (puzzle, kind) recordType records pageAsNumber recordsCount allPuzzles
    recordHandler currentUser userSlug recordId = do
        user <- grabOrNotFound $ Db.runDb $ Db.getUserBySlug userSlug
        (record, singles) <- grabOrNotFound $ Db.runDb $ Db.getRecordById recordId
        unless ((recordUserId record) == userId user) $
            notFound
        puzzleKind <- grabOrNotFound $ Db.runDb $ Db.getPuzzleKindById (recordPuzzleId record)
        pure $ H.recordShowPage currentUser user record singles puzzleKind
    shareRecordHandler (LoggedIn currentUser) userSlug recordId = do
        user <- grabOrNotFound $ Db.runDb $ Db.getUserBySlug userSlug
        (record, singles) <- grabOrNotFound $ Db.runDb $ Db.getRecordById recordId
        pk <- grabOrNotFound $ Db.runDb $ Db.getPuzzleKindById $ recordPuzzleId record
        if user == currentUser then do
            appId <- fromMaybe "" <$> asks Config.facebookAppId
            redirect303 $ facebookShareLink appId user (record, singles) pk
        else
            unauthorized
    timerHandler currentUser slug = do
        puzzle <- grabOrNotFound $ Db.runDb $ Db.getPuzzleBySlug slug
        kind <- grabOrNotFound $ Db.runDb $ Db.getKindById $ puzzleKindId puzzle
        allPuzzles <- Db.runDb Db.getAllPuzzles
        pure $ H.timerPage currentUser (puzzle, kind) allPuzzles
    protectedHandlers puzzleId user = submitSingleHandler puzzleId user
                                 :<|> deleteSingleHandler puzzleId user
                                 :<|> updateSingleHandler puzzleId user
    grabOrNotFound :: CubemaniaApp (Maybe a) -> CubemaniaApp a
    grabOrNotFound x = do
        y <- x
        case y of
            Just a -> return a
            Nothing -> notFound

recordsApiHandler :: PuzzleId -> Maybe Int -> Maybe UserId -> CubemaniaApp [RecordWithSingles]
recordsApiHandler puzzleId _page userId =
    case userId of
        Nothing -> return []
        Just uid -> do
            list <- Db.runDb $ Db.getRecords uid puzzleId
            pure $ RecordWithSingles <$> list

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
