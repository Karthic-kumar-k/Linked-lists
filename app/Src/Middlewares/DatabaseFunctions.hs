{-# LANGUAGE OverloadedStrings #-}

module Src.Middlewares.DatabaseFunctions where

import Data.Bool (bool)
import Data.Text
import Control.Monad.State
import Control.Monad.Reader

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Postgresql
import Database.PostgreSQL.Simple

import Src.Core
import Src.Models

runDb :: AppConfig -> SqlPersistT IO a -> AppMonad a
runDb appConfig query =
  liftIO $ runSqlPool query (appDbPool appConfig)

insertUser :: User -> AppMonad (Either Text (Key User))
insertUser user = do
  appConfig <- ask
  runDb appConfig $ do
    maybeExistingUser <- getBy (UniqueEmailUser (userEmail user))
    case maybeExistingUser of
          Just obj  -> return $ Left $ (("User with this email already exists. " :: Text) <> ( pack $ show (entityVal obj)))
          Nothing -> Right <$> insert user

getUser :: Text -> AppMonad (Maybe (Entity User))
getUser email = do
  appConfig <- ask
  runDb appConfig $ getBy (UniqueEmailUser email)

getLink :: Text -> AppMonad (Maybe Link)
getLink email = do
  appConfig <- ask
  maybeEntity <- runDb appConfig $ getBy (UniqueEmailLink email)
  pure $ entityVal <$> maybeEntity

insertLink :: Link -> AppMonad (Either Text Link)
insertLink link = do
  appConfig <- ask
  _ <- runDb appConfig $ insert link
  result <- getLink $ linkEmail link
  pure $ case result of
    Nothing ->
      Left "No Record found. Insertion not happened"
    Just link ->
      Right link

updateLinkByEmail :: Text -> Text -> AppMonad (Either Text Link)
updateLinkByEmail email urls = do
  appConfig <- ask
  _ <- runDb appConfig $ do
    updateWhere [LinkEmail ==. email] [LinkUrl =.urls]
  result <- getLink email
  pure $ case result of
    Nothing ->
      Left "No Record found. Result not updated"
    Just linkObj ->
      bool
      (Left "New Value is not updated. Persisting old value")
      (Right linkObj)
      ((linkUrl linkObj) == urls)
