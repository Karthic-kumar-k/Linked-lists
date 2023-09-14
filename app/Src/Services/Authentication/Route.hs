{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Src.Services.Authentication.Route where

import Servant
import Data.Text

import Src.Services.Authentication.Types

type API = "test" :> Get '[PlainText] Text
      :<|> "v1" :> "signin" :>  Post '[PlainText] Text
      :<|> "v1" :> "login" :> ReqBody '[JSON, FormUrlEncoded] LoginRequest :> Post '[JSON] LoginResponse

api :: Proxy API
api = Proxy