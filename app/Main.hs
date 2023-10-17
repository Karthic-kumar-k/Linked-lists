{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Servant

import qualified Database.Redis as Redis
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Postgresql
import Database.PostgreSQL.Simple

import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except


import Data.ByteString.Builder (byteString)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseBuilder)
import Network.Wai.Handler.Warp (run)

import Network.Wai.Logger (withStdoutLogger, ApacheLogger)

import Src.Core
import Src.Environment
import Src.Middlewares.DatabaseFunctions
import Src.Services.Handler
import Src.Services.Routes
import Src.Models

-- Initialize the application state with a database connection pool.
initializeAppConfig :: IO AppConfig
initializeAppConfig = do
  postgresConfigs <- getPostgresConfig
  redisConfigs <- getRedisConfigs
  postgresPool <- maybe (error "Can't find postgreSQL configs") makePool (pgConfigToString <$> postgresConfigs)
  redisPool <- maybe (error "Can't find redis configs") Redis.checkedConnect redisConfigs
  return $ AppConfig postgresPool redisPool

-- Create a connection pool for database connections
makePool :: String -> IO ConnectionPool
makePool connectionString = runStdoutLoggingT $ createPostgresqlPool (BSC.pack connectionString) 10

runDbMigrations :: AppMonad ()
runDbMigrations = do
  appConfig <- ask
  runDb appConfig (runMigration migrateAll)

app :: AppConfig -> Application
app = logStdoutDev . serve api . handlers

main :: IO()
main = do
  appConfig <- initializeAppConfig
  _ <- runExceptT $ runReaderT runDbMigrations appConfig

  withStdoutLogger $ \logger -> do
    let settings = setPort 8081 $ setLogger logger defaultSettings
    runSettings settings (app appConfig)
