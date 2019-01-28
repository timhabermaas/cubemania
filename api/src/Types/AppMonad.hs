{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Types.AppMonad
    ( CubemaniaApp
    , runCubemania
    ) where

import Control.Monad.Reader
import Types.Configuration
import Control.Monad.Except
import Servant

newtype CubemaniaApp a
  = CubemaniaApp
  ( ReaderT Configuration Handler a
  ) deriving (Functor, Applicative, Monad, MonadReader Configuration,
              MonadError ServantErr, MonadIO)

runCubemania :: Configuration -> CubemaniaApp a -> Handler a
runCubemania config (CubemaniaApp app) = runReaderT app config
