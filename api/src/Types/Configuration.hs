module Types.Configuration
    ( Configuration(..),
      Environment(..),
    ) where

import Database.PostgreSQL.Simple (Connection)
import Control.Concurrent.STM.TChan (TChan)
import Types.Stores (WastedTimeStore)
import Types.Events
import Data.Pool
import qualified Data.Text as T

data Environment = Development | Production deriving (Eq)

data Configuration = Configuration
  { getPool :: Pool Connection
  , wastedTimeStore :: WastedTimeStore
  , eventChannel :: TChan Event
  , facebookAppId :: Maybe String
  , environment :: Environment
  , emailPassword :: T.Text
  }
