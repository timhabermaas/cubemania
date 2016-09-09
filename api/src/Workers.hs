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
    putStrLn "waiting for event"
    event <- atomically $ do
        event <- readTChan channel
        case event of
          SingleSubmitted userId single ->
            addWastedTime store userId (fromIntegral (submittedSingleTime single))
          _ ->
            return ()
        return event
    putStrLn $ "handled event " ++ (show event)
    wastedTimeThread channel store
