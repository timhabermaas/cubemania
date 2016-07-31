module Main where

import Lib
import System.Environment (getEnv)

main :: IO ()
main = do
  connString <- getEnv "DATABASE_URL"
  startApp connString
