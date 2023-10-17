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
import qualified Src.Middlewares.MailService as Mail
import qualified Src.Services.Authentication.Types as AT

import Src.Models as Models
import qualified Src.Environment as Env

signinHandler :: AT.SignInRequest -> AppMonad AT.SignInResponse
signinHandler req = do
  userM1 <- DF.getUserFromMail (AT.mailId req)
  case userM1 of
    Nothing -> do
       userM2 <- DF.getUserFromUserName (AT.username req)
       case userM2 of
        Nothing -> do
          otp <- Mail.generateOTP
          mailResE <- Mail.triggerOTP (AT.mailId req) otp
          pure $ case mailResE of
            Left err ->
              AT.SignInResponse 02 (toText err)
            Right _ ->
              AT.SignInResponse 00 "OTP triggered"
        Just user ->
          pure $ AT.SignInResponse 01 "username already exists"
    Just user ->
      pure $ AT.SignInResponse 01 "User already exists"
    where
      toText = T.pack . show

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