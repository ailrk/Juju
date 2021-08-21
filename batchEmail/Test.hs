{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

module Test where

import qualified Data.ByteString             as S
import qualified Data.ByteString.Lazy        as B
import qualified Data.Text                   as T
import           Network.HaskellNet.Auth
import           Network.HaskellNet.SMTP
import           Network.HaskellNet.SMTP.SSL
import           Network.Mail.Mime
import           System.Exit                 (die)
import           System.IO

server       = "smtp.gmail.com"
port         = 465
authType     = LOGIN
from         = "jimmy123good@gmail.com"
to           = "jimmy123good@gmail.com"
subject      = "Network.HaskellNet.SMTP Test :)"
plainBody    = "Hello world!"
htmlBody     = "<html><head></head><body><h1>Hello <i>world!</i></h1></body></html>"
attachments  = [] -- example [("application/octet-stream", "/path/to/file1.tar.gz), ("application/pdf", "/path/to/file2.pdf")]

username = "jimmy123good@gmail.com"
password = "hruzlstjygburgmk"

runTest = doSMTPSTARTTLS server $ \conn -> do
  authSuccess <- authenticate authType username password conn
  if authSuccess
     then do
      let mail = simpleMail' to from subject plainBody
      sendMail mail conn
    else die "Authentication failed."
