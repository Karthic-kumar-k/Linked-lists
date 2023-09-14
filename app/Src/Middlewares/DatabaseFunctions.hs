{-# LANGUAGE OverloadedStrings #-}

module Src.Middlewares.DatabaseFunctions where

import Data.Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Servant

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Postgresql
import Database.PostgreSQL.Simple

import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.State
import Control.Monad.Reader

import Data.ByteString.Builder (byteString)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseBuilder)
import Network.Wai.Handler.Warp (run)

import Network.Wai.Logger (withStdoutLogger, ApacheLogger)

import Src.Core
import Src.Models.User

runDb :: AppConfig -> SqlPersistT IO a -> AppMonad a
runDb appConfig query =
  liftIO $ runSqlPool query (appDbPool appConfig)

insertUser :: User -> AppMonad (Either Text (Key User))
insertUser user = do
  appConfig <- ask
  runDb appConfig $ do
    maybeExistingUser <- getBy (UniqueEmail (userEmail user))
    case maybeExistingUser of
          Just _  -> return $ Left "User with this email already exists."
          Nothing -> Right <$> insert user