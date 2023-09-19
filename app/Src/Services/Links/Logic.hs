{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Src.Services.Links.Logic where

import Src.Core

import Prelude
import Servant
import qualified Data.Aeson as Aeson
import Data.Text (Text, pack)
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.Encoding as DTLE

import qualified Src.Middlewares.DatabaseFunctions as DF
import qualified Src.Services.Links.Types as LT
import Src.Models as Models

listLinks :: LT.ListLinkRequest -> AppMonad LT.ListLinkResponse
listLinks req = do
  listM <- DF.getLink $ LT.listEmailId req
  case listM of
    Nothing -> pure $ LT.ListLinkResponse Nothing "No Links found"
    Just link ->
      let urls = Aeson.decode . DTLE.encodeUtf8 . DTL.fromStrict $ linkUrl link :: Maybe [Text]
      in pure $ LT.ListLinkResponse urls "Success"

putLinks :: LT.PutLinkRequest -> AppMonad LT.PutLinkResponse
putLinks req = do
  listM <- DF.getLink $ LT.postEmailId req
  maybe (insertLinkRequest req) (\link -> updateLinks req link) listM

insertLinkRequest :: LT.PutLinkRequest -> AppMonad LT.PutLinkResponse
insertLinkRequest req = do
  resEither <- DF.insertLink $ Link (LT.postEmailId req) (pack . show $ LT.postUrls req)
  case resEither of
    Left err -> pure $ LT.PutLinkResponse err (LT.postEmailId req)
    Right _ -> pure $ LT.PutLinkResponse "Success" (LT.postEmailId req)

updateLinks :: LT.PutLinkRequest -> Models.Link -> AppMonad LT.PutLinkResponse
updateLinks req link = do
  let existingLinksText = DTLE.encodeUtf8 . DTL.fromStrict $ Models.linkUrl link
      existingLinks = Aeson.decode existingLinksText :: Maybe [Text]
      newLink = maybe (LT.postUrls req) (\e -> e ++ (LT.postUrls req)) existingLinks
  resEither <- DF.updateLinkByEmail (LT.postEmailId req) (pack . show $ newLink)
  case resEither of
    Left err -> pure $ LT.PutLinkResponse err (LT.postEmailId req)
    Right _ -> pure $ LT.PutLinkResponse "Success" (LT.postEmailId req)

listlinkFromUserName :: Text -> AppMonad LT.ListLinkResponse
listlinkFromUserName userName = do
  userM <- DF.getUserFromUserName userName
  case userM of
    Nothing -> throwError $ err400
    Just user -> do
      linkM <-  DF.getLink $ userEmail user
      case linkM of
        Nothing -> pure $ LT.ListLinkResponse Nothing "No Links found"
        Just link ->
          let urls = Aeson.decode . DTLE.encodeUtf8 . DTL.fromStrict $ linkUrl link :: Maybe [Text]
          in pure $ LT.ListLinkResponse urls "Success"