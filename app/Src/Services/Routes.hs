{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Src.Services.Routes where

import Servant

import qualified Src.Services.Authentication.Route as AR
import qualified Src.Services.Links.Route as LR

type API = AR.AuthAPI
  :<|> LR.LinkAPI

api :: Proxy API
api = Proxy