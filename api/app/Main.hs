module Main where

import Lib
import System.Environment (getEnv, lookupEnv)

main :: IO ()
main = do
  connString <- getEnv "DATABASE_URL"
  appId <- lookupEnv "FACEBOOK_APP_ID"
  environment <- lookupEnv "ENV"
  startApp connString appId environment
