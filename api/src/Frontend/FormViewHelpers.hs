{-# LANGUAGE OverloadedStrings #-}

module Frontend.FormViewHelpers
    ( textareaWithErrors
    , FieldRequired(..)
    ) where

import qualified Data.Text as T
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Digestive (View, fieldInputText, absoluteRef, errors)
import Data.Monoid ((<>))

data FieldRequired = Required | Optional

textareaWithErrors :: T.Text -> View Html -> FieldRequired -> Html
textareaWithErrors fieldName form' required =
    let errorClass = if hasErrors form' then " error" else ""
        hasErrors = not . null . errors fieldName
        errorList = mapM_ (p ! class_ "inline-errors") (errors fieldName form')
        labelAnnotation Required = abbr ! A.title "required" $ "*"
        labelAnnotation Optional = return ()
    in
        li ! class_ ("text input" <> errorClass) $ do
            H.label ! A.class_ "label" ! A.for (H.toValue (absoluteRef fieldName form')) $ do
                "Content"
                labelAnnotation required
            textarea ! name (toValue $ absoluteRef fieldName form') ! rows "4" $ toHtml $ fieldInputText fieldName form'
            errorList
