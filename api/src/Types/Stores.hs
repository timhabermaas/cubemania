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

newWastedTimeStore :: STM WastedTimeStore
newWastedTimeStore = WastedTimeStore <$> newTVar Map.empty

addWastedTime :: WastedTimeStore -> UserId -> Integer -> STM ()
addWastedTime (WastedTimeStore t) userId time =
    modifyTVar' t (\map -> Map.alter (\oldTime -> Just $ maybe time (+time) oldTime) userId map)

removeWastedTime :: WastedTimeStore -> UserId -> Integer -> STM ()
removeWastedTime (WastedTimeStore t) userId time =
    modifyTVar' t (\map -> Map.alter (\oldTime -> Just $ maybe time ((-) time) oldTime) userId map)

getWastedTimeFor :: WastedTimeStore -> UserId -> STM (Maybe Integer)
getWastedTimeFor (WastedTimeStore t) userId = do
    map <- readTVar t
    return $ Map.lookup userId map
