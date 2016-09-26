module Types.Configuration
    ( Configuration(..)
    ) where

import Database.PostgreSQL.Simple (Connection)
import Control.Concurrent.STM.TChan (TChan)
import Types.Stores (WastedTimeStore)
import Types.Events
import Data.Pool
import Data.Text

data Configuration = Configuration
  { getPool :: Pool Connection
  , wastedTimeStore :: WastedTimeStore
  , eventChannel :: TChan Event
  , facebookAppId :: Maybe String
  }
