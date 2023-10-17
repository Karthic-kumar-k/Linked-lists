{-# LANGUAGE OverloadedStrings #-}

module Src.Services.Authentication.Logic where

import Src.Core

import Servant
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
  let userName = AT.newUsername req
      userEmail = AT.newMailId req
  userM1 <- DF.getUserFromMail userEmail
  case userM1 of
    Nothing -> do
       userM2 <- DF.getUserFromUserName userName
       case userM2 of
        Nothing -> do
          otp <- Mail.generateOTP
          mailResE <- Mail.triggerOTP userEmail otp
          case mailResE of
            Left err ->
              pure $ AT.SignInResponse "02" "Can't able to send at the moment" Nothing Nothing
            Right _ -> do
              redisResp <- DF.insertOTPInRedis (T.encodeUtf8 userName) (T.encodeUtf8 otp)
              pure $ case redisResp of
                Left _ ->
                  -- need to add logging functionality
                  AT.SignInResponse "03" "Can't able to send at the moment" Nothing Nothing
                Right _ ->
                  AT.SignInResponse "00" "OTP triggered" (Just userName) (Just userEmail)
        Just user ->
          pure $ AT.SignInResponse "01" "username already exists" (Just userName) Nothing
    Just user ->
      pure $ AT.SignInResponse "01" "User already exists" Nothing (Just userEmail)

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

verifyOTP :: AT.VerifyOTPRequest -> AppMonad AT.VerifyOTPResponse
verifyOTP req = do
  redisRespE <- DF.getOTPFromRedis (T.encodeUtf8 $ AT.userName req)
  case redisRespE of
    Left _ -> pure $ AT.VerifyOTPResponse "03" "Not verified."
    Right valueM ->
      maybe
      (pure $ AT.VerifyOTPResponse "02" "Not verified. No OTP triggered")
      (\v -> verifyOTP' req v)
      valueM

verifyOTP' :: AT.VerifyOTPRequest -> BS.ByteString -> AppMonad AT.VerifyOTPResponse
verifyOTP' req realOtp = do
  _ <- DF.deleteOTPFromRedis (T.encodeUtf8 $  AT.userName req)
  if (AT.otp req) == (T.decodeUtf8 realOtp)
    then do
      userE <- DF.insertUser $ Models.User (AT.userName req) (AT.userEmail req) (AT.newPassword req)
      pure $ case userE of
        Left err ->
          AT.VerifyOTPResponse "04" "Not verified."
        Right _ ->
          AT.VerifyOTPResponse "00" "OTP Verified."
    else
      pure $ AT.VerifyOTPResponse "02" "Incorrect OTP."
