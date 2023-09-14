{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Src.Core where

import Prelude
-- import Control.Monad.State
import Control.Monad.Reader
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Postgresql
import Database.PostgreSQL.Simple
import Servant
import qualified Network.Wai as Wai

newtype AppConfig = AppConfig{ appDbPool :: ConnectionPool}

type AppMonad a = ReaderT AppConfig IO a

newtype AppHandler a = AppHandler { runAppHandler :: AppConfig -> Handler a }
