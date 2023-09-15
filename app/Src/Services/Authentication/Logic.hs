{-# LANGUAGE OverloadedStrings #-}

module Src.Services.Authentication.Logic where

import Src.Core

import Servant
import Data.Text (Text)
-- import Control.Monad.Except (throwError)
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.Encoding as DTLE

import Src.Middlewares.DatabaseFunctions
import Src.Services.Authentication.Route
import Src.Services.Authentication.Types

import Src.Models.User

handlers :: AppConfig -> Server API
handlers appConfig =
  hoistServer api f handlers'
  where
    f :: ReaderT AppConfig (ExceptT ServerError IO) a -> Handler a
    f r = do
      eResult <- liftIO $ runExceptT $ runReaderT r appConfig
      case eResult of
        Left err  -> throwError err
        Right res -> pure res

-- Define your server
handlers' :: AppServer
handlers' = helloHandler
    :<|> helloHandler
    :<|> greetHandler

helloHandler :: AppMonad Text
helloHandler = pure "Hello, Haskell!"

greetHandler :: LoginRequest -> AppMonad LoginResponse
greetHandler req = do
  let newUser = User (username req) (mailId req)
  userIdEither <- insertUser newUser
  case userIdEither of
    Left err ->
      let errorMessage = DTLE.encodeUtf8 $ DTL.fromStrict err
      in throwError $ err400 { errBody = errorMessage}
    Right uId -> pure $
      LoginResponse
      { emailId = mailId req
      , accessToken = "wjdehdv3jgkf2kebfkjghreIWB2298v"
      , refreshToken = "29823ASXDHDBbrurfibIBNadiubwOION"
      }
  -- case username req of
  --   Just _ ->
  --     pure $ LoginResponse{
  --       accessToken = "wjdehdv3jgkf2kebfkjghreIWB2298v",
  --       refreshToken = "29823ASXDHDBbrurfibIBNadiubwOION"
  --     }
  --   Nothing -> throwError $ err400 { errBody = "please enter username" }