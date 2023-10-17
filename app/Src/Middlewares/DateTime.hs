module Src.Middlewares.DateTime where

import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock.POSIX

import Src.Core

getCurrentTime :: AppMonad POSIXTime
getCurrentTime = liftIO getPOSIXTime