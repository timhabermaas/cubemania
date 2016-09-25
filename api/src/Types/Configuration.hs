module Types.Configuration
    ( Configuration(..)
    ) where

import Database.PostgreSQL.Simple (Connection)
import Control.Concurrent.STM.TChan (TChan)
import Types.Stores (WastedTimeStore)
import Types.Events
import Data.Text

data Configuration = Configuration
  { getPool :: Connection
  , wastedTimeStore :: WastedTimeStore
  , eventChannel :: TChan Event
  , facebookAppId :: Maybe String
  }
