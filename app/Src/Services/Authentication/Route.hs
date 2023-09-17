{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Src.Services.Authentication.Route where

import Servant
import Data.Text (Text)

import qualified Src.Services.Authentication.Types as AT

type AuthAPI = "test" :> Get '[PlainText] Text
      :<|> "v1" :> "login" :> ReqBody '[JSON, FormUrlEncoded] AT.LoginRequest :> Post '[JSON] AT.LoginResponse

authApi :: Proxy AuthAPI
authApi = Proxy
