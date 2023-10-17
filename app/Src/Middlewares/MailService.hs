{-# LANGUAGE OverloadedStrings #-}

module Src.Middlewares.MailService where

import Src.Core
import qualified Src.Environment as Env

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Control.Exception as E
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Except as Err
import Network.Mail.SMTP as SMTP
import Network.Mail.Mime as Mime
import System.Random
import qualified Servant as Servant

triggerOTP :: T.Text -> T.Text -> AppMonad (Either E.SomeException ())
triggerOTP recipient otp =
  sendOTPViaMail $ formMailBody recipient otp

generateOTP :: AppMonad T.Text
generateOTP = do
  rng <- liftIO newStdGen
  let from = 100000 :: Int
      to = 999999 :: Int
      (randomNumber, _) = randomR (from, to) rng
  pure . T.pack . show $ randomNumber

formMailBody :: T.Text -> T.Text -> Mime.Mail
formMailBody recipientMail otp =
  let sender = Address (Just "Linked Lists") "thelinkedlists@gmail.com"
      recipient = Address Nothing recipientMail
      subject = "OTP for Authenticaton"
      bodyText = "Signin Verification OTP : " <> otp
  in SMTP.simpleMail sender [recipient] [] [] subject [Mime.plainPart $ TL.fromStrict bodyText]

sendOTPViaMail ::Mime.Mail -> AppMonad (Either E.SomeException ())
sendOTPViaMail mailContent = do
  let smtpServer = "smtp.gmail.com"
      smtpPort = 587
      smtpUsername = "thelinkedlists@gmail.com"
  passM <- Env.getMailIdPassword
  case passM of
    Nothing -> Err.throwError $ Servant.err400 {Servant.errBody = "can't find password"}
    Just smtpPassword ->
      liftIO . E.try $ sendMailWithLoginTLS smtpServer smtpUsername smtpPassword mailContent
