{-# LANGUAGE OverloadedStrings          #-}

module Frontend.Forms
    ( commentForm
    , runGetForm
    , runPostForm
    ) where

import Types
import qualified Text.Digestive as DF
import qualified Data.Text as T

commentForm :: Monad m => DF.Form T.Text m SubmittedComment
commentForm = SubmittedComment <$> "content" DF..: DF.check "can't be blank" (greaterN 0) (DF.text Nothing)
  where
    greaterN :: Int -> T.Text -> Bool
    greaterN n t = T.length t > n

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
