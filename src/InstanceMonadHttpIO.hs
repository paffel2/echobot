{-# OPTIONS_GHC -fno-warn-orphans #-}

module InstanceMonadHttpIO where

import Control.Exception (throwIO)
import Network.HTTP.Req (MonadHttp(handleHttpException))

instance MonadHttp IO where
    handleHttpException = throwIO
