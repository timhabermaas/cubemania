{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Html
    ( usersPage
    , rootPage
    ) where

import Data.Monoid ((<>))
import Types
import Routes
import Data.Maybe (fromMaybe, isJust)
import Data.ByteString.Lazy (toStrict)
import Control.Monad (forM_, unless)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Markdown (markdown, def)

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
      selectedClass item = if item == currentPage then "selected" else ""
      navigationItem item = li ! class_ (toValue (navigationClass item <> " " <> selectedClass item)) $ a ! href (toValue $ navigationLink item) $ toHtml $ navigationText item
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
usersPage users maxSinglesCount currentPageNumber query = withLayout Users "Users" $ do
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
    fontSize SimpleUser{..} max = fromIntegral simpleUserSinglesCount / fromIntegral max * 1.4 + 0.6

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
