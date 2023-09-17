{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Src.Services.Links.Route where

import Servant

import qualified Src.Services.Links.Types as LT

type LinkAPI = "v1" :> "put" :> ReqBody '[JSON] LT.PutLinkRequest :> Post '[JSON] LT.PutLinkResponse
      :<|> "v1" :> "list" :> ReqBody '[JSON] LT.ListLinkRequest :> Post '[JSON] LT.ListLinkResponse

linkApi :: Proxy LinkAPI
linkApi = Proxy