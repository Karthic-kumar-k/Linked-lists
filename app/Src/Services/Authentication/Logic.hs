{-# LANGUAGE OverloadedStrings #-}

module Src.Services.Authentication.Logic where

import Src.Core

import Servant
import Data.Text (Text)
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.Encoding as DTLE

import qualified Src.Middlewares.DatabaseFunctions as DF
import qualified Src.Services.Authentication.Types as AT

import qualified Src.Models as Models

helloHandler :: AppMonad Text
helloHandler = pure "Hello, Haskell!"

greetHandler :: AT.LoginRequest -> AppMonad AT.LoginResponse
greetHandler req = do
  let newUser = Models.User (AT.username req) (AT.mailId req) (Just $ AT.phoneNumber req)
  userIdEither <- DF.insertUser newUser
  case userIdEither of
    Left err ->
      let errorMessage = DTLE.encodeUtf8 $ DTL.fromStrict err
      in throwError $ err400 { errBody = errorMessage}
    Right uId -> pure $
      AT.LoginResponse
      { AT.emailId = AT.mailId req
      , AT.accessToken = "wjdehdv3jgkf2kebfkjghreIWB2298v"
      , AT.refreshToken = "29823ASXDHDBbrurfibIBNadiubwOION"
      }