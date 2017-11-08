{-# LANGUAGE OverloadedStrings #-}

module Frontend.FormViewHelpers
    ( textareaWithErrors
    , textFieldWithErrors
    , textFieldWithDifferentErrors
    , passwordFieldWithErrors
    , passwordFieldWithDifferentErrors
    , checkbox
    , FieldRequired(..)
    ) where

import qualified Data.Text as T
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Digestive (View, fieldInputText, absoluteRef, errors)
import Data.Monoid ((<>))
import Control.Monad (join)

data FieldRequired = Required | Optional

checkbox :: T.Text -> T.Text -> View Html -> Html
checkbox ref labelName view =
    let ref' = toValue $ absoluteRef ref view
    in
        li ! class_ "boolean input optional" $ do
            H.label ! class_ "" ! for "user_wants_emails" $ do
                input ! A.id ref' ! name ref' ! type_ "checkbox"
                toMarkup labelName

-- TODO: Can this be replaced with childErrors from Text.Digestive.View?
textFieldWithDifferentErrors' :: T.Text -> [T.Text] -> T.Text -> View Html -> FieldRequired -> T.Text -> Html
textFieldWithDifferentErrors' fieldName errorNames labelName form' required type' =
    let errorClass = if hasErrors form' then " error" else ""
        hasErrors form'' = not $ null $ (join $ fmap (\en -> errors en form'') errorNames)
        errorList = mapM_ (p ! class_ "inline-errors") (join $ fmap (\en -> errors en form') errorNames)
        labelAnnotation Required = abbr ! A.title "required" $ "*"
        labelAnnotation Optional = return ()
    in
        li ! class_ (toValue $ "text input" <> errorClass <> " " <> fieldName) $ do
            H.label ! A.class_ "label"
                    ! A.for (H.toValue (absoluteRef fieldName form')) $ do
                toHtml labelName
                labelAnnotation required
            input ! name (toValue $ absoluteRef fieldName form')
                  ! type_ (toValue type')
                  ! value (toValue $ fieldInputText fieldName form')
            errorList

textFieldWithDifferentErrors :: T.Text -> [T.Text] -> T.Text -> View Html -> FieldRequired -> Html
textFieldWithDifferentErrors fieldName errorNames labelName form' required =
    textFieldWithDifferentErrors' fieldName errorNames labelName form' required "text"


textFieldWithErrors :: T.Text -> T.Text -> View Html -> FieldRequired -> Html
textFieldWithErrors fieldName = textFieldWithDifferentErrors fieldName [fieldName]

passwordFieldWithErrors :: T.Text -> T.Text -> View Html -> Html
passwordFieldWithErrors fieldName labelName form' = textFieldWithDifferentErrors' fieldName [fieldName] labelName form' Required "password"

passwordFieldWithDifferentErrors :: T.Text -> [T.Text] -> T.Text -> View Html -> Html
passwordFieldWithDifferentErrors fieldName errorNames labelName form' = textFieldWithDifferentErrors' fieldName errorNames labelName form' Required "password"


textareaWithErrors :: T.Text -> T.Text -> View Html -> FieldRequired -> Html
textareaWithErrors fieldName labelName form' required =
    let errorClass = if hasErrors form' then " error" else ""
        hasErrors = not . null . errors fieldName
        errorList = mapM_ (p ! class_ "inline-errors") (errors fieldName form')
        labelAnnotation Required = abbr ! A.title "required" $ "*"
        labelAnnotation Optional = return ()
    in
        li ! class_ ("text input" <> errorClass) $ do
            H.label ! A.class_ "label"
                    ! A.for (H.toValue (absoluteRef fieldName form')) $ do
                toHtml labelName
                labelAnnotation required
            textarea ! name (toValue $ absoluteRef fieldName form')
                     ! rows "4" $
                toHtml $ fieldInputText fieldName form'
            errorList
