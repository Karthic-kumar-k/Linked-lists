{-# LANGUAGE OverloadedStrings #-}

module Src.Services.Handler where

import Src.Core

import Servant
import Control.Monad.Reader
import Control.Monad.Except

import qualified Src.Services.Routes as SR
import qualified Src.Services.Authentication.Logic as AL
import qualified Src.Services.Links.Logic as LL

handlers :: AppConfig -> Server SR.API
handlers appConfig =
  hoistServer SR.api f handlers'
  where
    f :: ReaderT AppConfig (ExceptT ServerError IO) a -> Handler a
    f r = do
      eResult <- liftIO $ runExceptT $ runReaderT r appConfig
      case eResult of
        Left err  -> throwError err
        Right res -> pure res

handlers' :: AppServer
handlers' =
  AL.signinHandler :<|> AL.loginHandler :<|> AL.verifyOTP :<|> LL.putLinks :<|> LL.listLinks :<|> LL.listlinkFromUserName