{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RecordWildCards            #-}

module Frontend.Forms
    ( commentForm
    , Frontend.Forms.postForm
    , registerForm
    , editUserForm
    , loginForm
    , resetPasswordForm
    , runGetForm
    , runPostForm
    ) where

import Types
import Types.TimeZones
import Types.Configuration
import Db
import Text.Digestive as DF
import Control.Monad (join)
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

editUserForm :: (Monad m, MonadIO m, MonadReader Configuration m) => Maybe User -> Bool -> DF.Form T.Text m SubmittedEditUser
editUserForm user isAdmin =
    let emailText = (\(Email e) -> e) <$> userEmail <$> user
        ignoredField =
            if isAdmin then
                Just <$> (DF.bool (userIgnored <$> user))
            else
                pure Nothing
    in
        SubmittedEditUser <$> "name" .: validateUniqueUserNameIfChanged (userName <$> user) (validateSimpleCharacters (mustBePresent (text $ userName <$> user)))
                          <*> "email" .: validateUniqueEmailIfChanged (userEmail <$> user) (mustBePresent (text emailText))
                          <*> "wcaId" .: text (join $ userWca <$> user)
                          <*> "timeZone" .: timeZone (userTimeZone <$> user)
                          <*> "password" .: validateOptionalPassword
                          <*> "ignored" .: ignoredField

validateSimpleCharacters :: (Monad m) => DF.Form T.Text m T.Text -> DF.Form T.Text m T.Text
validateSimpleCharacters form =
    let allowedCharacters = ['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['0'..'9']
        errorMessage = "may only contain letters (a-Z), underscore (_) and digits (0-9)"
        validText t = T.all (\c -> c `elem` allowedCharacters) t
    in
        validate (\t -> if validText t then DF.Success t else DF.Error errorMessage) form

-- TODO: Da wir nur die Fehler von "password" bei p2 anzeigen, gibt's da kein "mustBePresent" mehr...
validatePassword :: (Monad m) => DF.Form T.Text m ClearPassword
validatePassword =
    validateEqual ("p1" .: mustBePresent (text Nothing)) ("p2" .: mustBePresent (text Nothing)) (\x _ -> ClearPassword x)

validateOptionalPassword :: (Monad m) => DF.Form T.Text m (Maybe ClearPassword)
validateOptionalPassword =
    validateEqual ("p1" .: text Nothing) ("p2" .: text Nothing) (\x _ -> Just $ ClearPassword x)

validateEqual :: (Monad m) => DF.Form T.Text m T.Text -> DF.Form T.Text m T.Text -> (T.Text -> T.Text -> a) -> DF.Form T.Text m a
validateEqual f1 f2 fun =
    validate equalityCheck $ (,) <$> f1 <*> f2
  where
    equalityCheck (x, y)
      | x == y = DF.Success $ fun x y
      | otherwise = DF.Error "must match password"

validateUniqueUserNameIfChanged :: (Monad m, MonadIO m, MonadReader Configuration m) => Maybe T.Text -> DF.Form T.Text m T.Text -> DF.Form T.Text m T.Text
validateUniqueUserNameIfChanged oldName =
    DF.validateM $ \name ->
        if Just name == oldName then
            pure $ DF.Success name
        else do
            user <- Db.runDb $ Db.getUserByName name
            pure $ maybe (DF.Success name) (const $ DF.Error "already exists") user

validateUniqueEmailIfChanged :: (Monad m, MonadIO m, MonadReader Configuration m) => Maybe Email -> DF.Form T.Text m T.Text -> DF.Form T.Text m Email
validateUniqueEmailIfChanged oldEmail =
    DF.validateM $ \email ->
        if Just (Email email) == oldEmail then
            pure $ DF.Success $ Email email
        else do
            user <- Db.runDb $ Db.getUserByEmail (Email email)
            pure $ maybe (DF.Success $ Email email) (const $ DF.Error "already exists") user

registerForm :: (Monad m, MonadIO m, MonadReader Configuration m) => DF.Form T.Text m SubmittedUser
registerForm =
    snd <$> ((,) <$> botCheck <*> userForm)
  where
    userForm = SubmittedUser <$> "name" .: validateUniqueUserNameIfChanged Nothing (validateSimpleCharacters (mustBePresent (text Nothing)))
                             <*> "email" .: validateUniqueEmailIfChanged Nothing (mustBePresent (text Nothing))
                             -- TODO: regex
                             <*> "wcaId" .: text Nothing
                             <*> "timeZone" .: timeZone Nothing
                             <*> "password" .: validatePassword
    botCheck = "bot" .: check "bots are not allowed" (== "") (text Nothing)

timeZone :: (Monad m) => Maybe T.Text -> DF.Form T.Text m T.Text
timeZone def = choice timeZones def

loginForm :: (Monad m) => DF.Form T.Text m SubmittedLogin
loginForm = SubmittedLogin <$> "name" .: mustBePresent (text Nothing)
                           <*> "password" .: (ClearPassword <$> mustBePresent (text Nothing))

resetPasswordForm :: (Monad m) => DF.Form T.Text m SubmittedResetPassword
resetPasswordForm = SubmittedResetPassword <$> "email" .: (Email <$> mustBePresent (text Nothing))



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
