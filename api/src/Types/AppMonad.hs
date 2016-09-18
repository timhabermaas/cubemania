{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Types.AppMonad
    ( CubemaniaApp
    , runCubemania
    , convertApp
    ) where

import Control.Monad.Reader
import Types.Configuration
import Control.Monad.Except
import Servant

newtype CubemaniaApp a
  = CubemaniaApp
  ( ReaderT Configuration (ExceptT ServantErr IO) a
  ) deriving (Functor, Applicative, Monad, MonadReader Configuration,
              MonadError ServantErr, MonadIO)

runCubemania :: Configuration -> CubemaniaApp a -> ExceptT ServantErr IO a
runCubemania config (CubemaniaApp app) = runReaderT app config

convertApp :: Configuration -> CubemaniaApp :~> ExceptT ServantErr IO
convertApp cfg = Nat $ runCubemania cfg
