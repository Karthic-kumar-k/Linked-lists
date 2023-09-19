{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Src.Services.Routes where

import Data.Text
import Servant

import qualified Src.Services.Authentication.Types as AT
import qualified Src.Services.Links.Types as LT

type API =
    "test" :> Get '[PlainText] Text
    :<|> "v1" :> "login" :> ReqBody '[JSON, FormUrlEncoded] AT.LoginRequest :> Post '[JSON] AT.LoginResponse
    :<|> "v1" :> "put" :> ReqBody '[JSON] LT.PutLinkRequest :> Post '[JSON] LT.PutLinkResponse
    :<|> "v1" :> "list" :> ReqBody '[JSON] LT.ListLinkRequest :> Post '[JSON] LT.ListLinkResponse
    :<|> "g" :> Capture "userName" Text :> Get '[JSON] LT.ListLinkResponse

api :: Proxy API
api = Proxy