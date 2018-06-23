{-# LANGUAGE OverloadedStrings #-}

module Frontend.FormViewHelpers
    ( textareaWithErrors
    , textFieldWithErrors
    , textFieldWithDifferentErrors
    , passwordFieldWithErrors
    , passwordFieldWithDifferentErrors
    , timeZoneField
    , checkbox
    , FieldRequired(..)
    ) where

import qualified Data.Text as T
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Digestive (View, fieldInputText, fieldInputBool, fieldInputChoice, absoluteRef, errors)
import Data.Monoid ((<>))
import Control.Monad (join)

data FieldRequired = Required | Optional

checkbox :: T.Text -> T.Text -> View Html -> Html
checkbox ref labelName view =
    let ref' = toValue $ absoluteRef ref view
        checkedAttribute = if fieldInputBool ref view then A.checked "" else mempty
    in
        li ! class_ "boolean input optional" $ do
            H.label ! class_ "" ! for "user_wants_emails" $ do
                input ! A.id ref' ! name ref' ! type_ "checkbox" ! checkedAttribute
                toMarkup labelName

-- TODO: Can this be replaced with childErrors from Text.Digestive.View?
textFieldWithDifferentErrors' :: T.Text -> [T.Text] -> T.Text -> View Html -> FieldRequired -> T.Text -> Html
textFieldWithDifferentErrors' fieldName errorNames labelName form' isRequired type' =
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
                labelAnnotation isRequired
            input ! name (toValue $ absoluteRef fieldName form')
                  ! type_ (toValue type')
                  ! value (toValue $ fieldInputText fieldName form')
            errorList

textFieldWithDifferentErrors :: T.Text -> [T.Text] -> T.Text -> View Html -> FieldRequired -> Html
textFieldWithDifferentErrors fieldName errorNames labelName form' isRequired =
    textFieldWithDifferentErrors' fieldName errorNames labelName form' isRequired "text"


textFieldWithErrors :: T.Text -> T.Text -> View Html -> FieldRequired -> Html
textFieldWithErrors fieldName = textFieldWithDifferentErrors fieldName [fieldName]

passwordFieldWithErrors :: T.Text -> T.Text -> View Html -> Html
passwordFieldWithErrors fieldName labelName form' = textFieldWithDifferentErrors' fieldName [fieldName] labelName form' Required "password"

passwordFieldWithDifferentErrors :: T.Text -> [T.Text] -> T.Text -> View Html -> Html
passwordFieldWithDifferentErrors fieldName errorNames labelName form' = textFieldWithDifferentErrors' fieldName errorNames labelName form' Required "password"

timeZoneField :: T.Text -> T.Text -> View Html -> Html
timeZoneField fieldName labelName form' =
    li ! class_ "time_zone input optional" ! A.id "user_time_zone_input" $ do
        H.label ! class_ " label" ! for ref $ toMarkup labelName
        select ! A.id ref ! name ref $
            mapM_ entry (fieldInputChoice fieldName form')
  where
    ref = toValue $ absoluteRef fieldName form'
    entry (v, l, selected) = option ! value (toValue v) ! (if selected then A.selected "" else mempty) $ l

textareaWithErrors :: T.Text -> T.Text -> View Html -> FieldRequired -> Html
textareaWithErrors fieldName labelName form' isRequired =
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
                labelAnnotation isRequired
            textarea ! name (toValue $ absoluteRef fieldName form')
                     ! rows "4" $
                toHtml $ fieldInputText fieldName form'
            errorList
