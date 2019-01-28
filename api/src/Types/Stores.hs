module Types.Stores
    ( WastedTimeStore
    , newWastedTimeStore
    , addWastedTime
    , removeWastedTime
    , getWastedTimeFor
    ) where

import Prelude hiding (map)
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM
import Types

newtype WastedTimeStore = WastedTimeStore (TVar (Map.Map UserId Integer))

newWastedTimeStore :: Map.Map UserId Integer -> STM WastedTimeStore
newWastedTimeStore initialMap = WastedTimeStore <$> newTVar initialMap

addWastedTime :: WastedTimeStore -> UserId -> Int -> STM ()
addWastedTime (WastedTimeStore s) uId time =
    modifyTVar' s (\map -> Map.alter (\oldTime -> Just $ maybe time' (+time') oldTime) uId map)
  where
    time' = fromIntegral time

removeWastedTime :: WastedTimeStore -> UserId -> Int -> STM ()
removeWastedTime (WastedTimeStore s) uId time =
    modifyTVar' s (\map -> Map.alter (\oldTime -> Just $ maybe time' (flip (-) time') oldTime) uId map)
  where
    time' = fromIntegral time

getWastedTimeFor :: WastedTimeStore -> UserId -> STM (Maybe Integer)
getWastedTimeFor (WastedTimeStore s) uId = do
    map <- readTVar s
    return $ Map.lookup uId map
