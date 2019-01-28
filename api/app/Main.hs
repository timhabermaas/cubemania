module Main where

import Lib
import System.Environment (getEnv, lookupEnv)

main :: IO ()
main = do
  connString <- getEnv "DATABASE_URL"
  appId <- lookupEnv "FACEBOOK_APP_ID"
  environment <- lookupEnv "ENV"
  emailPassword <- lookupEnv "EMAIL_PASSWORD"
  startApp connString appId environment emailPassword
