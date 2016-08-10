{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
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
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler, AuthServerData)
import MyServantBlaze
import Text.Blaze.Html5 (Html)

import Data.ByteString (split, filter, isPrefixOf)
import Data.ByteString.Char8 (unpack)
import Data.Char (ord)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromMaybe)

import Types
import qualified Html as H

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Except
import Database.PostgreSQL.Simple (connectPostgreSQL)

newtype MyStack a
  = MyStack
  { runApp :: ReaderT Configuration (ExceptT ServantErr IO) a
  } deriving (Functor, Applicative, Monad, MonadReader Configuration,
              MonadError ServantErr, MonadIO)

type PuzzleAPI = "api" :> "puzzles" :> Capture "puzzleId" PuzzleId :>
                      ("singles" :> QueryParam "user_id" UserId :> QueryParam "limit" Limit :> Get '[JSON] [Single]
                  :<|> "records" :> QueryParam "page" Int :> QueryParam "user_id" UserId :> Get '[JSON] [Record]
                  :<|> "singles" :> "chart.json" :> QueryParam "from" Float :> QueryParam "to" Float :> QueryParam "user_id" UserId :> Get '[JSON] [ChartData]
                  :<|> "singles" :> AuthProtect "cookie-auth" :> ReqBody '[JSON] SubmittedSingle :> PostNoContent '[JSON] NoContent
                  :<|> "singles" :> AuthProtect "cookie-auth" :> Capture "singleId" SingleId :> DeleteNoContent '[JSON] NoContent
                  :<|> "singles" :> AuthProtect "cookie-auth" :> Capture "singleId" SingleId :> ReqBody '[JSON] SubmittedSingle :> Put '[JSON] NoContent)
type CubemaniaAPI = PuzzleAPI
               :<|> "api" :> "users" :> QueryParam "q" String :> Get '[JSON] [SimpleUser] -- TODO: Remove .json
               :<|> "users" :> Get '[HTML] Html


type instance AuthServerData (AuthProtect "cookie-auth") = UserId

startApp :: String -> IO ()
startApp dbConnectionString = do
  conn <- connectPostgreSQL $ pack dbConnectionString --"postgresql://localhost:5432/cubemania_production"
  let c = Configuration conn
  run 9090 $ logStdoutDev $ app c

convertApp :: Configuration -> MyStack :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

appToServer :: Configuration -> Server CubemaniaAPI
appToServer cfg = enter (convertApp cfg) allHandlers

unauthorized :: MonadError ServantErr m => m a
unauthorized = throwError (err401 { errBody = "Missing auth cookie" })

authHandler :: AuthHandler Request UserId
authHandler =
    mkAuthHandler handler
  where
    handler req = do
      case lookup "Cookie" (requestHeaders req) of
        Just x -> do
          let foo = split (fromIntegral $ ord ';') x
          let bar = Data.ByteString.filter (\x -> fromIntegral x /= ord ' ') <$> foo
          let bar2 = safeHead $ split (fromIntegral $ ord '=') <$> Prelude.filter (\x -> isPrefixOf "current_user_id" x) bar
          liftIO $ putStrLn $ show bar2
          case bar2 >>= listToPair of
            Just (_key, value) ->
              case safeRead $ unpack value of
                Just x -> return $ UserId x
                Nothing -> unauthorized
            Nothing -> unauthorized
        Nothing -> unauthorized

    safeRead :: String -> Maybe Int
    safeRead x =
      case reads x of
        [(i, _rest)] -> Just i
        _ -> Nothing

    listToPair :: [a] -> Maybe (a, a)
    listToPair [a, b] = Just (a, b)
    listToPair _      = Nothing

apiContext :: Context (AuthHandler Request UserId ': '[])
apiContext = authHandler :. EmptyContext

app :: Configuration -> Application
app config = serveWithContext api apiContext $ appToServer config

api :: Proxy CubemaniaAPI
api = Proxy

allHandlers :: ServerT CubemaniaAPI MyStack
allHandlers = puzzleHandler :<|> usersHandler :<|> otherHandler
  where
    puzzleHandler puzzleId = singlesHandler puzzleId
                        :<|> recordsHandler puzzleId
                        :<|> chartHandler puzzleId
                        :<|> submitSingleHandler puzzleId
                        :<|> deleteSingleHandler puzzleId
                        :<|> updateSingleHandler puzzleId
    otherHandler = do
        users <- Db.runDb Db.getUsers
        maxSinglesCount <- Db.runDb Db.maxSinglesCount
        return $ H.usersPage users $ fromMaybe 1 maxSinglesCount


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

submitSingleHandler :: PuzzleId -> UserId -> SubmittedSingle -> MyStack NoContent
submitSingleHandler p uid s = do
    _ <- Db.runDb $ Db.postSingle p uid s
    return NoContent

updateSingleHandler :: PuzzleId -> UserId -> SingleId -> SubmittedSingle -> MyStack NoContent
updateSingleHandler p uid sid s = do
    _ <- Db.runDb $ Db.updateSingle p sid s
    return NoContent

deleteSingleHandler :: PuzzleId -> UserId -> SingleId -> MyStack NoContent
deleteSingleHandler _puzzleId userId singleId = do
    single <- Db.runDb $ Db.getSingle singleId
    if (singleUserId single) == userId then
      do
        Db.runDb $ Db.deleteSingle singleId
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
    epochToUTCTime = posixSecondsToUTCTime . fromIntegral . floor

usersHandler :: Maybe String -> MyStack [SimpleUser]
usersHandler q = do
    case q of
        Just query -> Db.runDb $ Db.matchUsers query
        Nothing    -> return []
