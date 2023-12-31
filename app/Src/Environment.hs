{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Src.Environment where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as Char8
import qualified Database.Redis as Redis
import System.Environment.Blank

import Src.Core

data PostgreSQLConfig = PostgreSQLConfig
  { pg_host :: String
  , pg_port :: String
  , pg_user :: String
  , pg_password :: String
  , pg_dbname :: String
  } deriving (Show, Eq)

pgConfigToString :: PostgreSQLConfig -> String
pgConfigToString c = "host=" <> pg_host c <> " port=" <> pg_port c <> " user=" <> pg_user c <> " password=" <> pg_password c <> " dbname=" <> pg_dbname c

getPostgresConfig :: IO (Maybe PostgreSQLConfig)
getPostgresConfig = do
  a <- getEnv "POSTGRES_HOST"
  b <- getEnv "POSTGRES_PORT"
  c <- getEnv "POSTGRES_USER"
  d <- getEnv "POSTGRES_PASS"
  e <- getEnv "POSTGRES_DB"

  return $ case (a,b,c,d,e) of
    (Just h, Just p, Just u, Just pa, Just da) -> Just $ PostgreSQLConfig h p u pa da
    _ -> Nothing
  -- PostgreSQLConfig
  --   <$> getEnv "POSTGRES_HOST"
  --   <*> getEnv "POSTGRES_PORT"
  --   <*> getEnv "POSTGRES_USER"
  --   <*> getEnv "POSTGRES_PASS"
  --   <*> getEnv "POSTGRES_DB"

getAccessTokenSecret :: AppMonad (Maybe String)
getAccessTokenSecret = liftIO $ getEnv "ACCESS_TOKEN_SECRET"

getMailIdPassword :: AppMonad (Maybe String)
getMailIdPassword = liftIO $ getEnv "MAIL_ID_PASSWORD"

getRedisConfigs :: IO (Maybe Redis.ConnectInfo)
getRedisConfigs = do
  a <- getEnv "REDIS_HOST"
  b <- getEnv "REDIS_PASS"

  return $ case (a, b) of
    (Just h, Just pa) ->
      Just $ Redis.defaultConnectInfo
        { Redis.connectHost = h
        , Redis.connectPort = Redis.PortNumber 6379
        , Redis.connectAuth = Char8.pack <$> b
        }
    _ ->
      Just Redis.defaultConnectInfo
        { Redis.connectHost = "127.0.0.1"
        , Redis.connectPort = Redis.PortNumber 6379
        }
