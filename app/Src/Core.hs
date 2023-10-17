{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Src.Core where

import Prelude

import Control.Monad.Reader
import Control.Monad.Except

import qualified Database.Redis as Redis
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Postgresql
import Database.PostgreSQL.Simple

import Servant
import qualified Network.Wai as Wai

import Src.Services.Routes (API(..))

data AppConfig = AppConfig
  { appDbPool :: ConnectionPool
  , redisConnection :: Redis.Connection
  }

type AppMonad = ReaderT AppConfig (ExceptT ServerError IO)

type AppServer = ServerT API AppMonad
