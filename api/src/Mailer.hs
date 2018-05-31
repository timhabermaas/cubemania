{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Mailer
    ( sendMail
    , registerMail
    , resetPasswordMail
    ) where

import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.Mail.Mime
import Types.Configuration
import Network.Mail.Client.Gmail
import Data.Monoid ((<>))
import Types


data Mail = Mail { mailTo :: Address, mailSubject :: T.Text, mailContent :: T.Text }

unlinesText :: [T.Text] -> T.Text
unlinesText = T.intercalate "\n"

resetPasswordMail :: Email -> T.Text -> ClearPassword -> Mailer.Mail
resetPasswordMail (Email email) userName (ClearPassword pw) =
    Mailer.Mail (Address (Just userName) email) "New Password for Cubemania" $
      unlinesText
        [ "Hi " <> userName <> ","
        , ""
        , "your new password is:"
        , pw
        , ""
        , "Your username: " <> userName
        , ""
        , "You can now login at https://www.cubemania.org/login using your new password."
        , "Happy cubing :)."
        ]

registerMail :: Email -> T.Text -> Mailer.Mail
registerMail (Email email) userName =
    Mailer.Mail (Address (Just userName) email) "Welcome to Cubemania!" $
      unlinesText [ "Hi " <> userName <> ","
      , ""
      , "thanks for signing up to cubemania.org."
      , ""
      , "We hope Cubemania can assist you in your long and hard way to the holy grail of speedcubing by:"
      , "* providing a nice graph which keeps you motivated"
      , "* letting you compare yourself to your cubing friends/enemies for even more motivation"
      , "* keeping track of your best times for every puzzle"
      , ""
      , "Why don't you go ahead and start practicing? https://www.cubemania.org/puzzles/3x3x3/timer"
      , ""
      , ""
      , "If you need any help, feel free to contact us on facebook: http://facebook.com/cubemania or by just replying to this email."
      , ""
      , "Happy cubing! :)"
      ]

sendMail :: (MonadIO m, MonadReader Configuration m) => Mailer.Mail -> m ()
sendMail mail = do
    password <- asks emailPassword
    liftIO $ sendGmail "info@cubemania.org" (TL.fromStrict password) (Address (Just "Cubemania") "info@cubemania.org") [Mailer.mailTo mail] [] [] (mailSubject mail) (TL.fromStrict $ mailContent mail) [] 10000000
