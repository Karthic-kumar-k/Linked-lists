{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Src.Core where

import Prelude
import Control.Monad.State
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Postgresql
import Database.PostgreSQL.Simple
import Servant
import qualified Network.Wai as Wai

newtype AppState = AppState{ appDbPool :: ConnectionPool}

type AppMonad a = StateT AppState IO a

newtype AppHandler a = AppHandler { runAppHandler :: AppState -> Handler a }
