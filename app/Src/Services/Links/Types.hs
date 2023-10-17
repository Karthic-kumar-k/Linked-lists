{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Src.Services.Links.Types where

import Prelude
import Data.Aeson
import Web.FormUrlEncoded
import GHC.Generics
import Data.Text

data ListLinkRequest = ListLinkRequest
  { listAccessToken :: Text
  , listEmailId     :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ListLinkRequest where
  parseJSON = withObject "ListLinkRequest" $ \obj -> do
    listAccessToken <- obj .: "accessToken"
    listEmailId <- obj .: "emailId"
    return ListLinkRequest { listAccessToken = listAccessToken, listEmailId = listEmailId }

instance ToJSON ListLinkRequest where
  toJSON (ListLinkRequest listAccessToken listEmailId) =
    object [ "accessToken" .= listAccessToken
           , "emailId" .= listEmailId
           ]

data ListLinkResponse = ListLinkResponse
  { listUrls :: Maybe [Text]
  , listMessage :: Text
  }
  deriving stock (Show,Eq,Generic)

instance ToJSON ListLinkResponse where
  toJSON (ListLinkResponse listUrls listMessage) =
    object [ "urls" .= toJSON listUrls
           , "message" .= listMessage
           ]

data PutLinkRequest = PutLinkRequest
  { postUrls :: [Text]
  , postEmailId :: Text
  }
  deriving stock (Show,Eq,Generic)

instance FromJSON PutLinkRequest where
  parseJSON = withObject "PutLinkRequest" $ \obj -> do
    postUrls <- obj .: "urls"
    postEmailId <- obj .: "emailId"
    return PutLinkRequest { postUrls = postUrls, postEmailId = postEmailId }

instance ToJSON PutLinkRequest where
  toJSON (PutLinkRequest postUrls postEmailId) =
    object [ "urls" .= postUrls
           , "emailId" .= postEmailId
           ]

data PutLinkResponse = PutLinkResponse
  { putMessage :: Text
  , postResEmailId :: Text
  }
  deriving stock (Show,Eq,Generic)

instance ToJSON PutLinkResponse where
  toJSON (PutLinkResponse postResEmailId putMessage) =
    object [ "emailId" .= postResEmailId
           , "message" .= putMessage
           ]