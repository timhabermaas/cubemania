{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}

module Frontend.Forms
    ( commentForm
    , Frontend.Forms.postForm
    , registerForm
    , loginForm
    , runGetForm
    , runPostForm
    ) where

import Types
import Types.Configuration
import Types.TimeZones
import Db
import Text.Digestive as DF
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader)
import qualified Data.Text as T

mustBePresent :: (Monad m) => Form T.Text m T.Text -> Form T.Text m T.Text
mustBePresent = check "can't be blank" (greaterN 0)
  where
    greaterN :: Int -> T.Text -> Bool
    greaterN n t = T.length t > n

commentForm :: Monad m => DF.Form T.Text m SubmittedComment
commentForm = SubmittedComment <$> "content" .: mustBePresent (DF.text Nothing)

postForm :: Monad m => DF.Form T.Text m SubmittedAnnouncement
postForm = SubmittedAnnouncement <$> "title" .: mustBePresent (text Nothing)
                                 <*> "content" .: mustBePresent (text Nothing)

registerForm :: (Monad m, MonadIO m, MonadReader Configuration m) => DF.Form T.Text m SubmittedUser
registerForm =
    snd <$> ((,) <$> botCheck <*> userForm)
  where
    userForm = SubmittedUser <$> "name" .: validateUniqueUserName (mustBePresent (text Nothing))
                             <*> "email" .: validateUniqueEmail (mustBePresent (text Nothing))
                             -- TODO: regex
                             <*> "wcaId" .: text Nothing
                             <*> "timeZone" .: text Nothing
                             <*> "password" .: validatePassword
    botCheck = "bot" .: check "bots are not allowed" (== "") (text Nothing)
    -- TODO: Da wir nur die Fehler von "password" bei p2 anzeigen, gibt's da kein "mustBePresent" mehr...
    validatePassword = validate equalityCheck $ (,) <$> "p1" .: mustBePresent (text Nothing) <*> "p2" .: mustBePresent (text Nothing)
    equalityCheck (p1, p2)
      | p1 == p2 = DF.Success $ ClearPassword p1
      | otherwise = DF.Error "must match password"
    validateUniqueUserName = DF.validateM (\name -> maybe (DF.Success name) (const $ DF.Error "already exists") <$> (Db.runDb $ Db.getUserByName name))
    validateUniqueEmail = DF.validateM (\email -> maybe (DF.Success email) (const $ DF.Error "already exists") <$> (Db.runDb $ Db.getUserByEmail email))

loginForm :: (Monad m) => DF.Form T.Text m SubmittedLogin
loginForm = SubmittedLogin <$> "name" .: mustBePresent (text Nothing)
                           <*> "password" .: (ClearPassword <$> mustBePresent (text Nothing))



runGetForm :: Monad m => T.Text -> DF.Form T.Text m a -> m (DF.View T.Text)
runGetForm = DF.getForm

runPostForm :: Monad m => T.Text -> DF.Form T.Text m a -> [(T.Text, T.Text)] -> m (DF.View T.Text, Maybe a)
runPostForm name form input =
    DF.postForm name form (return . env)
  where
    env _encType path =
        let fullPath = DF.fromPath path
        in case lookup fullPath input of
            Just a -> return [DF.TextInput a]
            Nothing -> return []
