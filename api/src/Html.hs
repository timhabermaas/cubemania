{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Html
    ( usersPage
    , userPage
    , rootPage
    ) where

import Data.Monoid ((<>))
import Types
import Routes
import Data.Maybe (fromMaybe, isJust)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Base16 (encode)
import Control.Monad (forM_, unless)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Markdown (markdown, def)
import qualified Crypto.Hash.MD5 as MD5

data Page = Home | Timer | Users | Records | Session deriving (Eq)

navigationClass :: Page -> T.Text
navigationClass Session = "session"
navigationClass _       = ""

navigationItems :: [Page]
navigationItems = [Home, Timer, Users, Records, Session]

navigationText :: Page -> T.Text
navigationText Home = "Home"
navigationText Timer = "Timer"
navigationText Users = "Users"
navigationText Records = "Records"
navigationText Session = "Login"

navigationLink :: Page -> T.Text
navigationLink Home = "/"
navigationLink Timer = "/puzzles/3x3x3/timer"
navigationLink Users = usersLink Nothing
navigationLink Records = "/puzzles/3x3x3/records"
navigationLink Session = "/login"


withLayout :: Page -> T.Text -> Html -> Html
withLayout currentPage title' inner =
    let
      selectedClass item' = if item' == currentPage then "selected" else ""
      navigationItem item' = li ! class_ (toValue (navigationClass item' <> " " <> selectedClass item')) $ a ! href (toValue $ navigationLink item') $ toHtml $ navigationText item'
      navigation = nav ! class_ "main" $ ul $ mapM_ navigationItem navigationItems
      footer' =
        footer $ p $ do
            "Founded by"
            space
            a ! href "http://cubemania.org/users/tim" $ "Tim Habermaas"
            ","
            space
            a ! href "http://www.patrickstadler.de" ! A.title "Patrick Stadler's Website" $ "Patrick Stadler"
            space
            "and Simon Wacker."
    in
      docTypeHtml $ do
          H.head $ do
              meta ! httpEquiv "content-type" ! content "text/html;charset=utf-8"
              link ! rel "apple-touch-icon" ! href "apple-touch-icon.png"
              H.title $ toHtml $ title' `T.append` " · Cubemania"
              link ! href "/assets/app.css" ! media "screen" ! rel "stylesheet" ! type_ "text/css"
              script ! src "/assets/app.js" ! type_ "text/javascript" $ mempty
              preEscapedToHtml ("<!--[if lt IE 9]><script src=\"http://html5shim.googlecode.com/svn/trunk/html5.js\"></script><![endif]-->" :: String)
              meta ! content "authenticity_token" ! name "csrf-param"
              meta ! content "iF91XiNQPByT6XXpFtx7BmWSt3k5unBNNs1F7NMni1c=" ! name "csrf-token"
              script ! type_ "text/javascript" $ "var _gaq = _gaq || [];\n      _gaq.push(['_setAccount', 'UA-28649455-1']);\n      _gaq.push(['_setDomainName', 'cubemania.org']);\n      _gaq.push(['_trackPageview']);\n\n      (function() {\n        var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;\n        ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';\n        var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);\n      })();"
          body $ do
              header ! class_ "main" $ H.div ! class_ "center" $ do
                  h1 $ a ! href "/" $ "Cubemania"
                  q "Save The World - Solve The Puzzle"
              navigation
              H.div ! A.id "flash" ! class_ "flash notice" ! A.style "display:none" $ p mempty
              section ! A.id "content" $ H.div ! class_ "center" $ inner
              footer'
              script ! type_ "text/javascript" $ "var uvOptions = {};\n  (function() {\n    var uv = document.createElement('script'); uv.type = 'text/javascript'; uv.async = true;\n    uv.src = ('https:' == document.location.protocol ? 'https://' : 'http://') + 'widget.uservoice.com/XmjQy7dHIjHW3AR0O50Cyw.js';\n    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(uv, s);\n  })();"

-- TODO: use Text instead of String
usersPage :: [SimpleUser] -> Int -> PageNumber -> Maybe T.Text -> Html
usersPage users maxSinglesCount currentPageNumber query = withLayout Users "Users" $
    H.div ! class_ "users-container" $ do
        H.form ! acceptCharset "UTF-8" ! action (toValue (usersLink Nothing)) ! A.id "users-search" ! method "get" $
            input ! A.id "q" ! required "required" ! name "q" ! placeholder "Search" ! type_ "search" ! value (toValue (fromMaybe "" query))
        ul ! A.id "users" ! class_ "users" $ forM_ users (\u -> userLi u >> space)
        unless (isJust query) $
            H.div ! class_ "pagination" $ a ! href (toValue (usersLink (Just (nextPage currentPageNumber)))) $ "Show2 more"
  where
    userLi user@SimpleUser{..} = li ! A.style (stringValue $ "font-size: " ++ show (fontSize user maxSinglesCount) ++ "em") $ do
        a ! href (stringValue ("/users/" ++ simpleUserSlug)) $ toHtml simpleUserName
        space
        small ! class_ "singles" $ toHtml $ show simpleUserSinglesCount
    fontSize :: SimpleUser -> Int -> Float
    fontSize SimpleUser{..} maxSinglesCount' = fromIntegral simpleUserSinglesCount / fromIntegral maxSinglesCount' * 1.4 + 0.6

userPage :: User -> Html
userPage User{..} = withLayout Users "User" $
    H.div ! A.id "user" $ do
        H.div ! class_ "admin" $ mempty
        h1 $ do
            profileImage
            space
            toHtml userName
            small "has spent 16 days solving puzzles."
        wcaLink
        h3 "Activity"
        section ! A.id "activity" ! dataAttribute "activity" activityJSON $ mempty
        h3 "Records"
        ul ! class_ "records" $ do
            li ! class_ "record even" $ do
                H.div ! class_ "puzzle" $ do
                    H.div ! class_ "puzzle-image pos0" $ H.div ! class_ "kind-image pos1" $ mempty
                    H.span $ do
                        "2x2x2"
                        small "BLD"
                table $ do
                    thead $ tr mempty
                    tbody $ do
                        tr $ do
                            th ! class_ "type" $ "Single"
                            td $ strong "26.53s"
                        tr $ do
                            th ! class_ "type" $ "Average of 5"
                            td $ strong "39.31s"
                        tr ! class_ "last" $ do
                            th ! class_ "type" $ "Average of 12"
                            td $ small "None"
            li ! class_ "record odd" $ do
                H.div ! class_ "puzzle" $ do
                    H.div ! class_ "puzzle-image pos0" $ H.div ! class_ "kind-image pos2" $ mempty
                    H.span $ do
                        "2x2x2"
                        small "OH"
                table $ do
                    thead $ tr mempty
                    tbody $ do
                        tr $ do
                            th ! class_ "type" $ "Single"
                            td $ strong "11.66s"
                        tr $ do
                            th ! class_ "type" $ "Average of 5"
                            td $ strong "20.23s"
                        tr ! class_ "last" $ do
                            th ! class_ "type" $ "Average of 12"
                            td $ small "None"
            li ! class_ "record even" $ do
                H.div ! class_ "puzzle" $ do
                    H.div ! class_ "puzzle-image pos0" $ H.div ! class_ "kind-image pos0" $ mempty
                    H.span $ do
                        "2x2x2"
                        small mempty
                table $ do
                    thead $ tr mempty
                    tbody $ do
                        tr $ do
                            th ! class_ "type" $ "Single"
                            td $ strong "3.14s"
                        tr $ do
                            th ! class_ "type" $ "Average of 5"
                            td $ strong "5.23s"
                        tr ! class_ "last" $ do
                            th ! class_ "type" $ "Average of 12"
                            td $ strong "6.44s"
            li ! class_ "record odd" $ do
                H.div ! class_ "puzzle" $ do
                    H.div ! class_ "puzzle-image pos1" $ H.div ! class_ "kind-image pos1" $ mempty
                    H.span $ do
                        "3x3x3"
                        small "BLD"
                table $ do
                    thead $ tr mempty
                    tbody $ do
                        tr $ do
                            th ! class_ "type" $ "Single"
                            td $ strong "58.50s"
                        tr $ do
                            th ! class_ "type" $ "Average of 5"
                            td $ strong "1:06.72min"
                        tr ! class_ "last" $ do
                            th ! class_ "type" $ "Average of 12"
                            td $ strong "1:15.81min"
            li ! class_ "record even" $ do
                H.div ! class_ "puzzle" $ do
                    H.div ! class_ "puzzle-image pos1" $ H.div ! class_ "kind-image pos2" $ mempty
                    H.span $ do
                        "3x3x3"
                        small "OH"
                table $ do
                    thead $ tr mempty
                    tbody $ do
                        tr $ do
                            th ! class_ "type" $ "Single"
                            td $ strong "20.96s"
                        tr $ do
                            th ! class_ "type" $ "Average of 5"
                            td $ strong "26.57s"
                        tr ! class_ "last" $ do
                            th ! class_ "type" $ "Average of 12"
                            td $ strong "30.06s"
            li ! class_ "record odd" $ do
                H.div ! class_ "puzzle" $ do
                    H.div ! class_ "puzzle-image pos1" $ H.div ! class_ "kind-image pos0" $ mempty
                    H.span $ do
                        "3x3x3"
                        small mempty
                table $ do
                    thead $ tr mempty
                    tbody $ do
                        tr $ do
                            th ! class_ "type" $ "Single"
                            td $ strong "9.16s"
                        tr $ do
                            th ! class_ "type" $ "Average of 5"
                            td $ strong "12.90s"
                        tr ! class_ "last" $ do
                            th ! class_ "type" $ "Average of 12"
                            td $ strong "13.87s"
            li ! class_ "record even" $ do
                H.div ! class_ "puzzle" $ do
                    H.div ! class_ "puzzle-image pos2" $ H.div ! class_ "kind-image pos1" $ mempty
                    H.span $ do
                        "4x4x4"
                        small "BLD"
                table $ do
                    thead $ tr mempty
                    tbody $ do
                        tr $ do
                            th ! class_ "type" $ "Single"
                            td $ strong "5:02.69min"
                        tr $ do
                            th ! class_ "type" $ "Average of 5"
                            td $ strong "8:45.69min"
                        tr ! class_ "last" $ do
                            th ! class_ "type" $ "Average of 12"
                            td $ small "None"
            li ! class_ "record odd" $ do
                H.div ! class_ "puzzle" $ do
                    H.div ! class_ "puzzle-image pos2" $ H.div ! class_ "kind-image pos2" $ mempty
                    H.span $ do
                        "4x4x4"
                        small "OH"
                table $ do
                    thead $ tr mempty
                    tbody $ do
                        tr $ do
                            th ! class_ "type" $ "Single"
                            td $ strong "6:18.00min"
                        tr $ do
                            th ! class_ "type" $ "Average of 5"
                            td $ small "None"
                        tr ! class_ "last" $ do
                            th ! class_ "type" $ "Average of 12"
                            td $ small "None"
            li ! class_ "record even" $ do
                H.div ! class_ "puzzle" $ do
                    H.div ! class_ "puzzle-image pos2" $ H.div ! class_ "kind-image pos0" $ mempty
                    H.span $ do
                        "4x4x4"
                        small mempty
                table $ do
                    thead $ tr mempty
                    tbody $ do
                        tr $ do
                            th ! class_ "type" $ "Single"
                            td $ strong "44.38s"
                        tr $ do
                            th ! class_ "type" $ "Average of 5"
                            td $ strong "52.80s"
                        tr ! class_ "last" $ do
                            th ! class_ "type" $ "Average of 12"
                            td $ strong "58.96s"
            li ! class_ "record odd" $ do
                H.div ! class_ "puzzle" $ do
                    H.div ! class_ "puzzle-image pos3" $ H.div ! class_ "kind-image pos1" $ mempty
                    H.span $ do
                        "5x5x5"
                        small "BLD"
                table $ do
                    thead $ tr mempty
                    tbody $ do
                        tr $ do
                            th ! class_ "type" $ "Single"
                            td $ strong "11:51.16min"
                        tr $ do
                            th ! class_ "type" $ "Average of 5"
                            td $ small "None"
                        tr ! class_ "last" $ do
                            th ! class_ "type" $ "Average of 12"
                            td $ small "None"
            li ! class_ "record even" $ do
                H.div ! class_ "puzzle" $ do
                    H.div ! class_ "puzzle-image pos3" $ H.div ! class_ "kind-image pos0" $ mempty
                    H.span $ do
                        "5x5x5"
                        small mempty
                table $ do
                    thead $ tr mempty
                    tbody $ do
                        tr $ do
                            th ! class_ "type" $ "Single"
                            td $ strong "1:37.05min"
                        tr $ do
                            th ! class_ "type" $ "Average of 5"
                            td $ strong "1:53.13min"
                        tr ! class_ "last" $ do
                            th ! class_ "type" $ "Average of 12"
                            td $ strong "1:59.19min"
            li ! class_ "record odd" $ do
                H.div ! class_ "puzzle" $ do
                    H.div ! class_ "puzzle-image pos5" $ H.div ! class_ "kind-image pos0" $ mempty
                    H.span $ do
                        "7x7x7"
                        small mempty
                table $ do
                    thead $ tr mempty
                    tbody $ do
                        tr $ do
                            th ! class_ "type" $ "Single"
                            td $ strong "5:47.92min"
                        tr $ do
                            th ! class_ "type" $ "Average of 5"
                            td $ strong "6:05.70min"
                        tr ! class_ "last" $ do
                            th ! class_ "type" $ "Average of 12"
                            td $ strong "6:11.29min"
            li ! class_ "record even" $ do
                H.div ! class_ "puzzle" $ do
                    H.div ! class_ "puzzle-image pos9" $ H.div ! class_ "kind-image pos0" $ mempty
                    H.span $ do
                        "Megaminx"
                        small mempty
                table $ do
                    thead $ tr mempty
                    tbody $ do
                        tr $ do
                            th ! class_ "type" $ "Single"
                            td $ strong "3:28.43min"
                        tr $ do
                            th ! class_ "type" $ "Average of 5"
                            td $ strong "3:46.98min"
                        tr ! class_ "last" $ do
                            th ! class_ "type" $ "Average of 12"
                            td $ strong "3:59.05min"
  where
    profileImage =
      let url email = "http://gravatar.com/avatar/" <> hash email <> ".png?s=60"
          hash email = TE.decodeUtf8 $ encode $ MD5.hash (TE.encodeUtf8 email)
      in
          img ! class_ "profile-image" ! src (toValue $ url userEmail)
    wcaLink =
        H.div ! A.id "wca" $ do
            a ! href "http://www.worldcubeassociation.org" ! class_ "logo" $ mempty
            a ! href "http://www.worldcubeassociation.org/results/p.php?i=2007HABE01" $ "tim's World Cube Association Profile"
    activityJSON = "{\"2015-10-07 00:00:00\":5,\"2016-02-21 00:00:00\":4,\"2015-09-17 00:00:00\":18,\"2016-04-14 00:00:00\":3,\"2015-10-29 00:00:00\":2,\"2016-02-18 00:00:00\":1,\"2016-03-05 00:00:00\":9,\"2015-12-20 00:00:00\":1,\"2016-04-17 00:00:00\":6,\"2015-09-19 00:00:00\":55,\"2015-10-14 00:00:00\":5,\"2015-11-02 00:00:00\":2,\"2015-09-13 00:00:00\":40,\"2016-03-03 00:00:00\":2,\"2016-04-16 00:00:00\":1,\"2015-09-30 00:00:00\":9,\"2015-12-07 00:00:00\":14,\"2015-09-29 00:00:00\":29,\"2016-04-04 00:00:00\":2,\"2015-09-11 00:00:00\":8,\"2015-11-27 00:00:00\":24,\"2015-10-27 00:00:00\":2,\"2015-10-10 00:00:00\":7,\"2015-10-02 00:00:00\":15,\"2015-09-14 00:00:00\":5,\"2015-10-31 00:00:00\":12,\"2015-09-22 00:00:00\":15,\"2015-10-09 00:00:00\":5,\"2015-10-03 00:00:00\":13,\"2015-12-10 00:00:00\":5,\"2015-09-08 00:00:00\":14,\"2016-02-12 00:00:00\":3,\"2016-03-02 00:00:00\":9,\"2015-10-04 00:00:00\":36,\"2015-10-30 00:00:00\":17,\"2016-04-15 00:00:00\":2,\"2015-10-15 00:00:00\":6,\"2015-09-18 00:00:00\":7,\"2016-04-01 00:00:00\":15,\"2015-09-21 00:00:00\":17,\"2015-09-28 00:00:00\":5,\"2016-04-13 00:00:00\":3,\"2015-10-01 00:00:00\":16,\"2015-12-17 00:00:00\":6,\"2015-11-03 00:00:00\":6,\"2016-03-07 00:00:00\":17,\"2015-11-30 00:00:00\":1,\"2015-09-15 00:00:00\":2,\"2016-02-20 00:00:00\":1,\"2016-04-21 00:00:00\":8,\"2015-09-16 00:00:00\":1,\"2015-10-11 00:00:00\":26,\"2015-09-20 00:00:00\":7}"

rootPage :: Maybe Announcement -> Html
rootPage post = withLayout Home "Home" $ do
    -- withMaybe (return ()) $ announcementHtml <$> post
    fromMaybe noAnnouncement $ announcementHtml <$> post
    p ! class_ "introduction" $ do
        "You want to keep track of your times, compare yourself with others and become the best?\n  If so, Cubemania is the right place for you: "
        a ! href "/register" $ "Register"
        " now and get the record!"
    ul ! A.id "features" $ do
        li ! class_ "odd" $ do
            a ! href "/puzzles/3x3x3/timer" ! class_ "image" $ img ! alt "Timer" ! src "/assets/images/screenshots/timer.jpg"
            br
            p "Stop your times and submit your averages."
        li $ do
            a ! href "/users/tim" ! class_ "image" $ img ! alt "Puzzles" ! src "/assets/images/screenshots/puzzles.jpg"
            br
            p "Organize your times properly."
        li ! class_ "odd" $ do
            a ! href "/puzzles/3x3x3/timer" ! class_ "image" $ img ! alt "Chart" ! src "/assets/images/screenshots/chart.jpg"
            p "Keep track of your progress and compare yourself with other cubers."
        li $ do
            a ! href "/puzzles/3x3x3/records" ! class_ "image" $ img ! alt "Records" ! src "/assets/images/screenshots/records.jpg"
            p "Get the record!"
  where
    noAnnouncement = return ()
    announcementHtml Announcement{..} =
        article ! class_ "announcement" $ do
            strong $ toHtml announcementTitle
            space
            preEscapedToHtml $ renderInlineMarkdown announcementContent
            space
            a ! href (toValue $ postLinkToComments announcementId) $ "76 Comments »"

-- Type annotation necessary for ToMarkup class
space :: Html
space = toHtml (" " :: T.Text)

renderInlineMarkdown :: T.Text -> T.Text
renderInlineMarkdown = T.replace "</p>" "" . T.replace "<p>" "" . TE.decodeUtf8 . toStrict . renderHtml . markdown def . LT.fromStrict
