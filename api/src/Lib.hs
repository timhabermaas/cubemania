{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lib
    ( startApp
    ) where

import qualified Db as Db

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5 (Html)

import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.ByteString.Char8 (pack)
import Data.Maybe(fromMaybe)

import Types
import qualified Html as H

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Except
import Database.PostgreSQL.Simple

newtype MyStack a
  = MyStack
  { runApp :: ReaderT Configuration (ExceptT ServantErr IO) a
  } deriving (Functor, Applicative, Monad, MonadReader Configuration,
              MonadError ServantErr, MonadIO)

type PuzzleAPI = "api" :> "puzzles" :> Capture "puzzleId" PuzzleId :>
                      ("singles" :> QueryParam "user_id" UserId :> QueryParam "limit" Limit :> Get '[JSON] [Single]
                  :<|> "records" :> QueryParam "page" Int :> QueryParam "user_id" UserId :> Get '[JSON] [Record]
                  :<|> "singles" :> "chart.json" :> QueryParam "from" Float :> QueryParam "to" Float :> QueryParam "user_id" UserId :> Get '[JSON] [ChartData])
type CubemaniaAPI = PuzzleAPI
               :<|> "api" :> "users.json" :> QueryParam "q" String :> Get '[JSON] [SimpleUser] -- TODO: Remove .json
               :<|> "users" :> Get '[HTML] Html


startApp :: String -> IO ()
startApp dbConnectionString = do
  conn <- connectPostgreSQL $ pack dbConnectionString --"postgresql://localhost:5432/cubemania_production"
  let c = Configuration conn
  run 9090 $ logStdoutDev $ app c

convertApp :: Configuration -> MyStack :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

appToServer :: Configuration -> Server CubemaniaAPI
appToServer cfg = enter (convertApp cfg) allHandlers

app :: Configuration -> Application
app config = serve api $ appToServer config

api :: Proxy CubemaniaAPI
api = Proxy

allHandlers :: ServerT CubemaniaAPI MyStack
allHandlers = puzzleHandler :<|> usersHandler :<|> otherHandler
  where
    puzzleHandler puzzleId = singlesHandler puzzleId :<|> recordsHandler puzzleId :<|> chartHandler puzzleId
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
        Just uid -> Db.runDb $ Db.getSingles puzzleId uid $ getLimit limit
  where
    getLimit l = case l of
        Nothing -> Limit 150
        Just l' -> l'

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
