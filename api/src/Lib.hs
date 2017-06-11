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
import Mailer

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Web.Cookie (parseCookies)
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import System.Metrics
-- TODO: import qualified because of names like port, host, ...
import System.Remote.Monitoring.Statsd (forkStatsd, defaultStatsdOptions, StatsdOptions(..))

import Data.ByteString (ByteString, split, filter, isPrefixOf, null)
import Data.ByteString.Char8 (unpack, pack)
import Data.ByteString.Base64 as Base64
import qualified Data.CaseInsensitive as CI
import Data.Char (ord)
import Data.Either.Combinators (rightToMaybe)
import Data.Bifunctor (bimap)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UUID (toText)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.List (intercalate)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (newBroadcastTChan, dupTChan, writeTChan)

import Types
import qualified Types.Configuration as Config
import Types.Stores (newWastedTimeStore, getWastedTimeFor)
import Types.Events
import Routes
import qualified Html as H
import Workers (wastedTimeThread, emailWorkerThread)
import Frontend.Forms

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Except
import Database.PostgreSQL.Simple (connectPostgreSQL, close)
import Data.Pool (createPool, withResource)
import Types.AppMonad


startApp :: String -> Maybe String -> Maybe String -> Maybe String -> IO ()
startApp dbConnectionString facebookAppId env emailPassword = do
  -- Send GHC stats to statsd
  store <- newStore
  registerGcMetrics store
  --forkStatsd (defaultStatsdOptions { host = "statsd", port = 8125 }) store

  pool <- createPool (connectPostgreSQL $ pack dbConnectionString) close 1 10 10
  channel <- atomically newBroadcastTChan
  wastedTimeStore <- atomically newWastedTimeStore
  let env' = case env of
                 (Just "production") -> Config.Production
                 _                   -> Config.Development
      c = Config.Configuration pool wastedTimeStore channel facebookAppId env' (T.pack $ fromMaybe "" emailPassword)
      port = 9090
  putStrLn $ "Starting server on port " ++ show port

  wastedTimeChannel <- atomically $ dupTChan channel
  emailChannel <- atomically $ dupTChan channel
  forkIO $ wastedTimeThread wastedTimeChannel wastedTimeStore
  forkIO $ emailWorkerThread emailChannel c
  let makeSubmitEventFromSingle s = SingleSubmitted (singleUserId s) (SubmittedSingle (singleScramble s) (singleTime s) (singlePenalty s))
  let importEvents conn = Db.getAllSingles (\single -> atomically $ writeTChan wastedTimeChannel (makeSubmitEventFromSingle single)) conn

  _ <- forkIO (withResource pool importEvents >> putStrLn "finished importing singles")

  let logger = if env' == Config.Production then
                   logStdout
               else
                   logStdoutDev
  run port $ logger $ app c

appToServer :: Config.Configuration -> Server CubemaniaRoutes
appToServer cfg = enter (convertApp cfg) allHandlers

unauthorized :: MonadError ServantErr m => m a
unauthorized = throwError (err401 { errBody = "Missing auth cookie" })

unauthorizedWeb :: MonadError ServantErr m => m a
unauthorizedWeb = redirect303WithCookies "/" [("flash-message", "Please login or <a href=\"/register\">register</a> to continue.")]

unsetFlashMessage :: T.Text
unsetFlashMessage = "flash-message="

redirect303 :: MonadError ServantErr m => T.Text -> m a
redirect303 url = throwError $ err303 { errHeaders = [("Location", TE.encodeUtf8 url)] }

redirect303WithCookies :: MonadError ServantErr m => T.Text -> [(T.Text, T.Text)] -> m a
redirect303WithCookies url headers =
    throwError $ err303 { errHeaders = ("Location", TE.encodeUtf8 url) : (createCookieHeader . (bimap TE.encodeUtf8 TE.encodeUtf8) <$> headers) }
  where
    createCookieHeader (k, v) = ("Set-Cookie", k <> "=" <> Base64.encode v <> "; Path=/")

authHandler :: Config.Configuration -> AuthHandler Request LoggedInUser
authHandler configuration =
    mkAuthHandler $ runCubemania configuration . handler
  where
    handler :: Request -> CubemaniaApp LoggedInUser
    handler req = do
      maybeUser <- authHandlerOptional' req
      maybe unauthorizedWeb return maybeUser

getCookieFromRequest :: Request -> ByteString -> Maybe ByteString
getCookieFromRequest req s = do
    cookieValue <- lookup "Cookie" (requestHeaders req)
    let cookies = parseCookies cookieValue
    -- Pretend empty cookies aren't valid.
    -- TODO: Handle deleting cookies properly by setting expires flag.
    rawValue <- snd <$> (safeHead $ Prelude.filter (\(l, _) -> l == s) cookies)
    decodedValue <- rightToMaybe $ Base64.decode rawValue
    mfilter (not . Data.ByteString.null) (Just decodedValue)

authHandlerOptional :: Config.Configuration -> AuthHandler Request (Maybe (LoggedIn User))
authHandlerOptional configuration =
    mkAuthHandler $ runCubemania configuration . authHandlerOptional'

authHandlerOptional' :: Request -> CubemaniaApp (Maybe LoggedInUser)
authHandlerOptional' req = do
    case getCookieFromRequest req "my-session" >>= sessionIdFromByteString of
      Just s -> do
          session <- Db.runDb $ Db.readSession s
          case session of
              Just (SerializedSessionData userId) ->
                  fmap (\u -> LoggedIn u s) <$> Db.runDb (Db.getUserById userId)
              Nothing -> return Nothing
      Nothing -> return Nothing

flashHandler :: AuthHandler Request (Maybe FlashMessage)
flashHandler = mkAuthHandler $ foo
  where
    foo req = do
        case getCookieFromRequest req "flash-message" of
            Just s -> pure $ Just $ FlashMessage $ TE.decodeUtf8 s
            Nothing -> pure Nothing

apiContext :: Config.Configuration -> Context (AuthHandler Request (Maybe (LoggedIn User))  ': AuthHandler Request (LoggedIn User) ': AuthHandler Request (Maybe FlashMessage) ': '[])
apiContext config = (authHandlerOptional config) :. (authHandler config)  :. (flashHandler) :. EmptyContext

app :: Config.Configuration -> Application
app config = serveWithContext api (apiContext config) $ appToServer config

allHandlers :: ServerT CubemaniaRoutes CubemaniaApp
allHandlers
    = jsonApiHandler
 :<|> (\flashMessage -> usersHandler flashMessage
                   :<|> userHandler flashMessage
                   :<|> postsHandler flashMessage
                   :<|> postHandler flashMessage
                   :<|> newPostHandler flashMessage
                   :<|> createPostHandler flashMessage
                   :<|> editPostHandler flashMessage
                   :<|> updatePostHandler flashMessage
                   :<|> postCommentHandler flashMessage
                   :<|> editUserHandler flashMessage
                   :<|> recordsHandler flashMessage
                   :<|> recordHandler flashMessage
                   :<|> shareRecordHandler flashMessage
                   :<|> timerHandler flashMessage
                   :<|> getRegisterHandler flashMessage
                   :<|> registerHandler flashMessage
                   :<|> getLoginHandler flashMessage
                   :<|> loginHandler flashMessage
                   :<|> logoutHandler flashMessage
                   :<|> rootHandler flashMessage)
  where
    jsonApiHandler = puzzleHandler :<|> usersApiHandler
    puzzleHandler puzzleId = singlesHandler puzzleId
                        :<|> recordsApiHandler puzzleId
                        :<|> chartHandler puzzleId
                        :<|> protectedHandlers puzzleId
    usersHandler flashMessage currentUser query page = do
        let pageNumber = fromMaybe (PageNumber 1) page
        users <- case query of
            Just q -> Db.runDb $ Db.matchUsers q
            Nothing -> Db.runDb $ Db.getUsers (fromPageNumber pageNumber)
        maxSinglesCount <- Db.runDb Db.maxSinglesCount
        return $ H.usersPage currentUser users (fromMaybe 1 maxSinglesCount) pageNumber query
    userHandler flashMessage currentUser userSlug = do
        let maybeUser (Just (LoggedIn u _)) = Just u
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
    postsHandler flashMessage currentUser = do
        posts <- Db.runDb $ Db.getAnnouncements
        foo <- mapM (\p -> do
                      author <- Db.runDb $ Db.getUserById $ announcementUserId p
                      commentsCount <- length <$> Db.runDb (Db.getCommentsForAnnouncement $ announcementId p)
                      pure (p, author, commentsCount)) posts
        pure $ H.postsPage currentUser foo
    newPostHandler flashMessage currentUser = do
        mustBeAdmin currentUser
        form <- runGetForm "post" postForm
        return $ H.newPostPage currentUser form
    createPostHandler flashMessage cu@(LoggedIn currentUser _) body = do
        mustBeAdmin cu
        form <- runPostForm "post" postForm body
        case form of
            (_, Just p) -> do
                pId <- Db.runDb $ Db.postAnnouncement (userId currentUser) p
                redirect303 $ postLinkToComments pId
            (view, Nothing) ->
                pure $ H.newPostPage cu view
    editPostHandler flashMessage currentUser aId = do
        mustBeAdmin currentUser
        Announcement{..} <- grabOrNotFound $ Db.runDb $ Db.getAnnouncement aId
        (view, _) <- runPostForm "post" postForm [("post.title", announcementTitle),
                                                  ("post.content", announcementContent)]
        pure $ H.editPostPage currentUser aId view
    updatePostHandler flashMessage currentUser aId body = do
        mustBeAdmin currentUser
        Announcement{..} <- grabOrNotFound $ Db.runDb $ Db.getAnnouncement aId
        form <- runPostForm "post" postForm body
        case form of
            (_, Just c) -> do
                Db.runDb $ Db.updateAnnouncement announcementId c
                redirect303 $ postLink announcementId
            (view, Nothing) -> do
                pure $ H.editPostPage currentUser announcementId view
    postHandler flashMessage currentUser pId = do
        form <- runGetForm "comment" commentForm
        post <- grabOrNotFound $ Db.runDb $ Db.getAnnouncement pId
        renderPostPage currentUser post form
    renderPostPage currentUser post form = do
        user <- Db.runDb $ Db.getUserById $ announcementUserId post
        comments' <- Db.runDb $ Db.getCommentsForAnnouncement $ announcementId post
        comments <- mapM (\c@Comment{..} -> (if isJust commentAuthorId then Db.runDb $ Db.getUserById (fromJust commentAuthorId) else return Nothing) >>= \u -> return (c, u)) comments'
        return $ H.postPage currentUser post user comments form
    postCommentHandler flashMessage lu@(LoggedIn currentUser _) pId body = do
        form <- runPostForm "comment" commentForm body
        post <- grabOrNotFound $ Db.runDb $ Db.getAnnouncement pId
        case form of
            (_, Just c) -> do
                Db.runDb $ Db.postComment (announcementId post) (userId currentUser) (submittedCommentContent c)
                redirect303 $ postLinkToComments (announcementId post)
            (view, Nothing) ->
                renderPostPage (Just lu) post view
    editUserHandler flashMessage currentUser userSlug = do
        user <- grabOrNotFound $ Db.runDb $ Db.getUserBySlug userSlug
        mustBeSelf currentUser user
        form <- runGetForm "user" editUserForm
        return $ H.editUserPage currentUser form
    getRegisterHandler flashMessage currentUser = do
        mustBeLoggedOut currentUser
        form <- runGetForm "user" registerForm
        return $ H.registerPage currentUser form
    registerHandler flashMessage currentUser body = do
        mustBeLoggedOut currentUser
        form <- runPostForm "user" registerForm body
        case form of
            (_, Just u) -> do
                (pw, salt) <- hashNewPassword $ submittedPassword u
                userId <- Db.runDb $ Db.createUser u salt pw
                (SessionId sessionId) <- Db.runDb $ Db.createSession (SerializedSessionData userId)
                publishEvent $ UserRegistered u
                redirect303WithCookies "/" [("flash-message", "Hello " <> submittedUserName u <> ", you are now registered."), ("my-session", toText sessionId)]
            (view, Nothing) ->
                return $ H.registerPage currentUser view
    getLoginHandler flashMessage currentUser = do
        mustBeLoggedOut currentUser
        form <- runGetForm "login" loginForm
        return $ runReader (H.loginPage currentUser form) Nothing
    logoutHandler flashMessage (LoggedIn user sessionId) = do
        Db.runDb $ Db.deleteSession sessionId
        redirect303WithCookies "/" [("my-session", ""), ("flash-message", "You are now logged out.")]
    loginHandler flashMessage currentUser body = do
        mustBeLoggedOut currentUser
        form <- runPostForm "login" loginForm body
        case form of
            (view, Just loginData) -> do
                user <- Db.runDb $ Db.getUserByName (submittedLoginName loginData)
                let errorMessage = "Username and password do not match."
                case user of
                    Just u ->
                        if authenticate (userSalt u, submittedLoginPassword loginData) (userPassword u) then do
                            (SessionId sessionId) <- Db.runDb $ Db.createSession (SerializedSessionData (userId u))
                            redirect303WithCookies "/" [("flash-message", "Hello " <> userName u <> ", you are now logged in."), ("my-session", toText sessionId)]
                        else
                            return $ renderFlashPage (H.loginPage currentUser view) errorMessage
                    Nothing -> do
                        return $ renderFlashPage (H.loginPage currentUser view) errorMessage
            (view, Nothing) ->
                return $ renderPage $ H.loginPage currentUser view
    rootHandler flashMessage currentUser cookie = do
        announcement <- Db.runDb Db.getLatestAnnouncement
        comments <- maybe (return []) (\a -> Db.runDb $ Db.getCommentsForAnnouncement (announcementId a)) announcement
        return $ addHeader unsetFlashMessage (runReader (H.rootPage currentUser ((\a -> (a, comments)) <$> announcement)) flashMessage)
    recordsHandler flashMessage currentUser slug type' page = do
        puzzle <- grabOrNotFound $ Db.runDb $ Db.getPuzzleBySlug slug
        -- TODO: join with puzzle fetch
        kind <- grabOrNotFound $ Db.runDb $ Db.getKindById $ puzzleKindId puzzle
        let pageAsNumber = fromMaybe 1 (fromPageNumber <$> page)
        let recordType = fromMaybe AverageOf5Record type'
        records <- Db.runDb $ Db.getRecordsForPuzzleAndType (puzzleId puzzle) recordType pageAsNumber
        recordsCount <- Db.runDb $ Db.getRecordCountForPuzzleAndType (puzzleId puzzle) recordType
        allPuzzles <- Db.runDb Db.getAllPuzzles
        pure $ H.recordsPage currentUser (puzzle, kind) recordType records pageAsNumber recordsCount allPuzzles
    recordHandler flashMessage currentUser userSlug recordId = do
        user <- grabOrNotFound $ Db.runDb $ Db.getUserBySlug userSlug
        (record, singles) <- grabOrNotFound $ Db.runDb $ Db.getRecordById recordId
        unless ((recordUserId record) == userId user) $
            notFound
        puzzleKind <- grabOrNotFound $ Db.runDb $ Db.getPuzzleKindById (recordPuzzleId record)
        pure $ H.recordShowPage currentUser user record singles puzzleKind
    shareRecordHandler flashMessage (LoggedIn currentUser _) userSlug recordId = do
        user <- grabOrNotFound $ Db.runDb $ Db.getUserBySlug userSlug
        (record, singles) <- grabOrNotFound $ Db.runDb $ Db.getRecordById recordId
        pk <- grabOrNotFound $ Db.runDb $ Db.getPuzzleKindById $ recordPuzzleId record
        if user == currentUser then do
            appId <- fromMaybe "" <$> asks Config.facebookAppId
            redirect303 $ facebookShareLink appId user (record, singles) pk
        else
            unauthorizedWeb
    timerHandler flashMessage currentUser slug = do
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
submitSingleHandler p (LoggedIn user _) s = do
    -- TODO: DB transaction
    singleId' <- Db.runDb $ Db.postSingle p (userId user) s
    single <- Db.runDb $ Db.getSingle singleId'
    publishEvent (SingleSubmitted (userId user) s)
    return $ addHeader "true" single

updateSingleHandler :: PuzzleId -> LoggedIn User -> SingleId -> SubmittedSingle -> CubemaniaApp NoContent
updateSingleHandler _puzzleId (LoggedIn user _) singleId' s = do
    single <- Db.runDb $ Db.getSingle singleId'
    if singleUserId single == userId user then
      do
        Db.runDb $ Db.updateSingle singleId' s
        return NoContent
    else
      unauthorized

deleteSingleHandler :: PuzzleId -> LoggedIn User -> SingleId -> CubemaniaApp NoContent
deleteSingleHandler _puzzleId (LoggedIn user _) singleId' = do
    single <- Db.runDb $ Db.getSingle singleId'
    if (singleUserId single) == (userId user) then do
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

renderPage page = runReader page Nothing
renderFlashPage page message = runReader page $ Just message

mustBeAdmin :: (MonadError ServantErr m) => LoggedIn User -> m ()
mustBeAdmin (LoggedIn User{..} _) =
    case userRole of
        AdminRole -> pure ()
        _ -> redirect303WithCookies "/" [("flash-message", "You do not have the necessary permissions!")]

mustBeLoggedOut :: (MonadError ServantErr m) => Maybe (LoggedIn User) -> m ()
mustBeLoggedOut Nothing = pure ()
mustBeLoggedOut _ = redirect303WithCookies "/" [("flash-message", "You must logout before you can login or register.")]

mustBeSelf :: (MonadError ServantErr m) => LoggedIn User -> User -> m ()
mustBeSelf (LoggedIn cu _) u =
    if cu == u then
        pure ()
    else
        redirect303WithCookies "/" [("flash-message", "You do not have the necessary permissions!")]
