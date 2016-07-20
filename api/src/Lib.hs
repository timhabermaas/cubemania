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

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant

import Data.Maybe (fromMaybe)

import Types

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Except
import Database.PostgreSQL.Simple

data Configuration = Configuration
  { getPool :: Connection
  }

newtype MyStack a
  = MyStack
  { runApp :: ReaderT Configuration (ExceptT ServantErr IO) a
  } deriving (Functor, Applicative, Monad, MonadReader Configuration,
              MonadError ServantErr, MonadIO)

type MyAPI = "api" :> "puzzles" :> Capture "puzzleId" PuzzleId :> "singles" :> QueryParam "user_id" UserId :> QueryParam "limit" Limit :> Get '[JSON] [Single]
        :<|> "api" :> "puzzles" :> Capture "puzzleId" PuzzleId :> "records" :> QueryParam "page" Int :> QueryParam "user_id" UserId :> Get '[JSON] [Record]


runDb :: (MonadReader Configuration m, MonadIO m) => (Connection -> m a) -> m a
runDb query = do
    conn <- asks getPool
    query conn

getRecords :: (MonadIO m) => UserId -> PuzzleId -> Connection -> m [Record]
getRecords (UserId uid) (PuzzleId pid) conn = do
    records <- liftIO $ query conn "select id, time, comment, puzzle_id, amount from records where user_id = ? and puzzle_id = ?" (uid, pid)
    liftIO $ mapM (grabSingles conn) records

grabSingles :: Connection -> Record -> IO Record
grabSingles conn r = do
    singles <- query conn "SELECT singles.id, singles.time, singles.scramble FROM singles INNER JOIN records_singles ON singles.id = records_singles.single_id WHERE records_singles.record_id = ?" (Only $ recordId r)
    return $ r { recordSingles = singles }

getSingles :: (MonadIO m) => PuzzleId -> UserId -> Limit -> Connection -> m [Single]
getSingles (PuzzleId pid) (UserId uid) (Limit limit) conn = do
    singles <- liftIO $ query conn "select id, time, comment, scramble, penalty, created_at from singles where puzzle_id = ? and user_id = ? ORDER BY created_at DESC LIMIT ?" (pid, uid, limit)
    liftIO $ putStrLn $ show $ length singles
    return singles

startApp :: IO ()
startApp = do
  conn <- connectPostgreSQL "postgresql://postgres@db/cubemania_production"
  let c = Configuration conn
  run 9090 $ logStdoutDev $ app c

convertApp :: Configuration -> MyStack :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

appToServer :: Configuration -> Server MyAPI
appToServer cfg = enter (convertApp cfg) allHandlers

app :: Configuration -> Application
app config = serve api $ appToServer config

api :: Proxy MyAPI
api = Proxy

allHandlers :: ServerT MyAPI MyStack
allHandlers = singlesHandler :<|> recordsHandler

recordsHandler :: PuzzleId -> Maybe Int -> Maybe UserId -> MyStack [Record]
recordsHandler puzzleId _page userId =
    case userId of
        Nothing -> return []
        Just uid -> runDb $ getRecords uid puzzleId

singlesHandler :: PuzzleId -> Maybe UserId -> Maybe Limit -> MyStack [Single]
singlesHandler puzzleId userId limit = do
    case userId of
        Nothing -> return []
        Just uid -> runDb $ getSingles puzzleId uid $ getLimit limit
  where
    getLimit l = case l of
        Nothing -> Limit 150
        Just l' -> l'
