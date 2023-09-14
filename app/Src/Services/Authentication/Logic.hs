{-# LANGUAGE OverloadedStrings #-}

module Src.Services.Authentication.Logic where

import Src.Core

import Servant
import Data.Text (Text)
import Control.Monad.Except (throwError)

import Src.Services.Authentication.Route
import Src.Services.Authentication.Types

-- Define your server
handlers :: Server API
handlers = helloHandler
    :<|> helloHandler
    :<|> greetHandler

helloHandler :: Handler Text
helloHandler = pure "Hello, Haskell!"

greetHandler :: LoginRequest -> Handler LoginResponse
greetHandler req =
  case username req of
    Just _ ->
      pure $ LoginResponse{
        accessToken = "wjdehdv3jgkf2kebfkjghreIWB2298v",
        refreshToken = "29823ASXDHDBbrurfibIBNadiubwOION"
      }
    Nothing -> throwError $ err400 { errBody = "please enter username" }