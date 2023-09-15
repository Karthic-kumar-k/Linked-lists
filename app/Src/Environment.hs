{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Src.Environment where

import Prelude

import System.Environment.Blank

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