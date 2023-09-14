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
import Src.Middlewares.DatabaseFunctions
import Src.Services.Authentication.Route
import Src.Services.Authentication.Logic
import Src.Models.User

-- Initialize the application state with a database connection pool.
initializeAppConfig :: IO AppConfig
initializeAppConfig = do
  pool <- makePool "host=localhost port=5432 user=postgres password=pass dbname=test"
  return $ AppConfig pool

-- Create a connection pool for database connections
makePool :: String -> IO ConnectionPool
makePool connectionString = runStdoutLoggingT $ createPostgresqlPool (BSC.pack connectionString) 10

runDbMigrations :: AppMonad ()
runDbMigrations = do
  appConfig <- ask
  runDb appConfig (runMigration migrateAll)

app :: Application
app = logStdoutDev $ serve api handlers

main :: IO()
main = do
  appConfig <- initializeAppConfig
  _ <- runReaderT runDbMigrations appConfig

  let newUser = User "deepa" "deepakumar@gmail.com"
  userIdEither <- runReaderT (insertUser newUser) appConfig
  case userIdEither of
    Left err -> putStrLn $ unpack err
    Right userId -> putStrLn $ "User inserted with ID: " ++ show userId

  withStdoutLogger $ \logger -> do
    let settings = setPort 8081 $ setLogger logger defaultSettings
    runSettings settings app
