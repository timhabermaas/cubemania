{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Html
    ( usersPage
    ) where

import Types
import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

usersPage :: [SimpleUser] -> Int-> Html
usersPage users maxSinglesCount =
    section ! A.id "content" $ H.div ! class_ "center" $ do
        H.form ! acceptCharset "UTF-8" ! action "/users" ! A.id "users-search" ! method "get" $ input ! A.id "q" ! name "q" ! placeholder "Search" ! type_ "search"
        ul ! A.id "users" ! class_ "users" $ forM_ users userLi
        H.div ! class_ "pagination" $ a ! href "http://localhost:3000/users?page=2" ! dataAttribute "remote" "true" $ "Show more"
  where
    userLi user@SimpleUser{..} = li ! A.style (stringValue $ "font-size: " ++ (show $ fontSize user maxSinglesCount) ++ "em") $ do
        a ! href (stringValue ("/users/" ++ simpleUserSlug)) $ toHtml simpleUserName
        small ! class_ "singles" $ toHtml $ show simpleUserSinglesCount
    fontSize SimpleUser{..} max = ((fromIntegral simpleUserSinglesCount) / (fromIntegral max)) * 1.4 + 0.6
