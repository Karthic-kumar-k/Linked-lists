{-# LANGUAGE OverloadedStrings #-}

module Src.Middlewares.Auth where

import qualified Data.Text as T
import qualified Web.JWT as JWT
import Servant

import Src.Core
import qualified Src.Environment as Env

verifyAuth :: T.Text -> AppMonad ()
verifyAuth token = do
  secret <- Env.getAccessTokenSecret
  case secret of
    Nothing -> throwError $ err500
    Just secretKey ->
      let sKey = JWT.toVerify . JWT.hmacSecret . T.pack $ secretKey
          mVerifiedJWT = JWT.decodeAndVerifySignature sKey token
      in
        case mVerifiedJWT of
          Nothing ->
            (throwError $ err403 {errBody = "Invalid Authorisation header"})
          Just _ -> pure ()