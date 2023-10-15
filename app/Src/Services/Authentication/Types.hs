{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Src.Services.Authentication.Types

where

import Prelude
import Data.Aeson
import Web.FormUrlEncoded
import GHC.Generics
import Data.Text

data LoginRequest = LoginRequest{
  username :: Text,
  mailId :: Text,
  phoneNumber :: Text,
  password :: Text
}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, FromForm, ToForm)

data LoginResponse = LoginResponse{
  accessToken :: Text,
  refreshToken :: Maybe Text,
  emailId :: Text
}
  deriving stock (Show,Eq,Generic)
  deriving anyclass (FromJSON, ToJSON, FromForm, ToForm)
