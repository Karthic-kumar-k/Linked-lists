{-# LANGUAGE OverloadedStrings #-}

module Src.Services.Authentication.Logic where

import Src.Core

import Servant
import qualified Data.Text as T
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.Encoding as DTLE
import qualified Web.JWT as JWT

import qualified Src.Middlewares.DatabaseFunctions as DF
import qualified Src.Middlewares.DateTime as Date
import qualified Src.Services.Authentication.Types as AT

import Src.Models as Models
import qualified Src.Environment as Env

helloHandler :: AppMonad T.Text
helloHandler = pure "Hello, Haskell!"

loginHandler :: AT.LoginRequest -> AppMonad AT.LoginResponse
loginHandler req = do
  userM <- DF.getUserFromMail (AT.mailId req)
  case userM of
    Nothing ->
      throwError $ err400 { errBody = "Invalid email/password"}
    Just user ->
      if (Models.userPassword user) == (AT.password req)
        then
          sendLoginResponse user
        else
          throwError $ err400 { errBody = "incorrect password"}

sendLoginResponse :: Models.User -> AppMonad AT.LoginResponse
sendLoginResponse user = do
  secret <- Env.getAccessTokenSecret
  case secret of
    Nothing -> throwError $ err500
    Just secretKey -> do
      iat <- Date.getCurrentTime
      let cs = mempty -- mempty returns a default JWTClaimsSet
                { JWT.iss = JWT.stringOrURI . T.pack $ "linkedlists.in"
                , JWT.sub = JWT.stringOrURI $ Models.userEmail user
                , JWT.iat = JWT.numericDate $ iat
                }
          sKey = JWT.hmacSecret . T.pack $ secretKey
          accessToken = JWT.encodeSigned sKey mempty cs
      pure $ AT.LoginResponse accessToken Nothing (Models.userEmail user)