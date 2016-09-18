module Workers
    ( wastedTimeThread
    ) where

import Types
import Types.Stores
import Types.Events
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan

wastedTimeThread :: TChan Event -> WastedTimeStore -> IO ()
wastedTimeThread channel store = do
    atomically $ do
        event <- readTChan channel
        case event of
          SingleSubmitted userId single ->
            addWastedTime store userId (submittedSingleTime single)
          SingleDeleted single ->
            removeWastedTime store (singleUserId single) (singleTime single)
          _ ->
            return ()
    wastedTimeThread channel store
