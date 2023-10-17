{-# LANGUAGE OverloadedStrings #-}

module Src.Middlewares.DatabaseFunctions where

import Data.Bool (bool)
import qualified Data.ByteString as BS
import Data.Text
import Control.Monad.State
import Control.Monad.Reader

import Database.Redis as Redis
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Postgresql
import Database.PostgreSQL.Simple

import Src.Core
import Src.Models

runDb :: AppConfig -> SqlPersistT IO a -> AppMonad a
runDb appConfig query =
  liftIO $ runSqlPool query (appDbPool appConfig)

-- insertUser :: User -> AppMonad (Either Text (Key User))
-- insertUser user = do
--   appConfig <- ask
--   runDb appConfig $ do
--     maybeExistingUser <- getBy (UniqueEmailUser (userEmail user))
--     case maybeExistingUser of
--           Just obj  -> return $ Left $ (("User with this email already exists. " :: Text) <> ( pack $ show (entityVal obj)))
--           Nothing -> Right <$> insert user

insertUser :: User -> AppMonad (Either Text (Key User))
insertUser user = do
  appConfig <- ask
  runDb appConfig $ do
    maybeUser <- insertUnique user
    case maybeUser of
          Just obj  -> pure $ Right obj
          Nothing -> return $ Left ("Email/UserName already exists." :: Text)

getUserFromMail :: Text -> AppMonad (Maybe User)
getUserFromMail email = do
  appConfig <- ask
  maybeEntity <- runDb appConfig $ getBy (UniqueEmailUser email)
  pure $ entityVal <$> maybeEntity

getUserFromUserName :: Text -> AppMonad (Maybe User)
getUserFromUserName userName = do
  appConfig <- ask
  maybeEntity <- runDb appConfig $ getBy (UniqueUsernameUser userName)
  pure $ entityVal <$> maybeEntity

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

insertOTPInRedis :: BS.ByteString -> BS.ByteString -> AppMonad (Either Redis.Reply Redis.Status)
insertOTPInRedis username otp = do
  configs <- ask
  liftIO . Redis.runRedis (redisConnection configs) $ Redis.set username otp

getOTPFromRedis :: BS.ByteString -> AppMonad (Either Redis.Reply (Maybe BS.ByteString))
getOTPFromRedis username = do
  configs <- ask
  liftIO . Redis.runRedis (redisConnection configs) $ Redis.get username

deleteOTPFromRedis :: BS.ByteString -> AppMonad (Either Redis.Reply Integer)
deleteOTPFromRedis username = do
  configs <- ask
  liftIO . Redis.runRedis (redisConnection configs) $ Redis.del [username]