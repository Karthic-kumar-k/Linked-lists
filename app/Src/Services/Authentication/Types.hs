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

data SignInRequest = SignInRequest
  { newUsername :: Text
  , newMailId :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, FromForm)

data SignInResponse = SignInResponse
  { resCode :: Text
  , message :: Text
  , name :: Maybe Text
  , email :: Maybe Text
  }
  deriving stock (Show,Eq,Generic)
  deriving anyclass (ToJSON)

data LoginRequest = LoginRequest
  { mailId :: Text
  , password :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, FromForm)

data LoginResponse = LoginResponse
  { accessToken :: Text
  , refreshToken :: Maybe Text
  , emailId :: Text
  }
  deriving stock (Show,Eq,Generic)
  deriving anyclass (ToJSON)

data VerifyOTPRequest = VerifyOTPRequest
  { otp :: Text
  , userName :: Text
  , userEmail :: Text
  , newPassword :: Text
  }
  deriving stock (Show,Eq,Generic)
  deriving anyclass (FromJSON, FromForm)

data VerifyOTPResponse = VerifyOTPResponse
  { responseCode :: Text
  , responseMessage :: Text
  }
  deriving stock (Show,Eq,Generic)
  deriving anyclass (ToJSON)