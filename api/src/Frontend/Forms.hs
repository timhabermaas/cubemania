{-# LANGUAGE OverloadedStrings          #-}

module Frontend.Forms
    ( commentForm
    , Frontend.Forms.postForm
    , runGetForm
    , runPostForm
    ) where

import Types
import Text.Digestive as DF
import qualified Data.Text as T

mustBePresent :: (Monad m) => Form T.Text m T.Text -> Form T.Text m T.Text
mustBePresent = check "can't be blank" (greaterN 0)
  where
    greaterN :: Int -> T.Text -> Bool
    greaterN n t = T.length t > n

commentForm :: Monad m => DF.Form T.Text m SubmittedComment
commentForm = SubmittedComment <$> "content" .: mustBePresent (DF.text Nothing)
  where
    greaterN :: Int -> T.Text -> Bool
    greaterN n t = T.length t > n

postForm :: Monad m => DF.Form T.Text m SubmittedAnnouncement
postForm = SubmittedAnnouncement <$> "title" .: mustBePresent (text Nothing)
                                 <*> "content" .: mustBePresent (text Nothing)

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
