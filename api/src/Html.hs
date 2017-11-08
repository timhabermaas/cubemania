{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Html
    ( usersPage
    , userPage
    , postPage
    , postsPage
    , newPostPage
    , editPostPage
    , recordsPage
    , recordShowPage
    , rootPage
    , timerPage
    ) where

import Data.Monoid ((<>))
import Types
import Routes
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map.Strict as Map
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
import Text.Digestive (View)
import qualified Data.Time.Format as DF
import qualified Data.Aeson as JSON
import qualified Crypto.Hash.MD5 as MD5
import Utils
import Frontend.FormViewHelpers
import Frontend.PuzzleNavigation
import Frontend.BackboneTemplates

data Page = Home | Timer | Users | Records deriving (Eq)

navigationItems :: [Page]
navigationItems = [Home, Timer, Users, Records]

navigationText :: Page -> T.Text
navigationText Home = "Home"
navigationText Timer = "Timer"
navigationText Users = "Users"
navigationText Records = "Records"

navigationLink :: Page -> T.Text
navigationLink Home = "/"
navigationLink Timer = "/puzzles/3x3x3/timer"
navigationLink Users = usersLink Nothing
navigationLink Records = "/puzzles/3x3x3/records"

withSubnavigationLayout :: Maybe LoggedInUser -> Page -> T.Text -> Maybe Html -> Html -> Html
withSubnavigationLayout currentUser currentPage title' subnav inner =
    let
      selectedClass item' = if item' == currentPage then "selected" else ""
      navigationItem item' = li ! class_ (toValue (selectedClass item' :: T.Text)) $
                                 a ! href (toValue $ navigationLink item') $
                                     toHtml $ navigationText item'
      sessionNavigation =
          case currentUser of
              Just (LoggedIn u) -> do
                  li ! class_ "session" $ a ! href "/logout" $ "Logout"
                  li ! class_ "session" $ a ! href (toValue $ userLink (userSlug u)) $ toHtml (userName u <> "'s Profile")
              Nothing ->
                  li ! class_ "session" $ a ! href "/login" $ "Login"
      navigation = nav ! class_ "main" $ ul $ mapM_ navigationItem navigationItems <> sessionNavigation
      footer' =
        footer $ p $ do
            "Founded by"
            space
            a ! href "/users/tim" $ "Tim Habermaas"
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
              preEscapedToHtml ("<!--[if lt IE 9]><script src=\"http://html5shim.googlecode.com/svn/trunk/html5.js\"></script><![endif]-->" :: T.Text)
              meta ! content "authenticity_token" ! name "csrf-param"
              meta ! content "iF91XiNQPByT6XXpFtx7BmWSt3k5unBNNs1F7NMni1c=" ! name "csrf-token"
              script ! type_ "text/javascript" $ "var _gaq = _gaq || [];\n      _gaq.push(['_setAccount', 'UA-28649455-1']);\n      _gaq.push(['_setDomainName', 'cubemania.org']);\n      _gaq.push(['_trackPageview']);\n\n      (function() {\n        var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;\n        ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';\n        var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);\n      })();"
          body $ do
              header ! class_ "main" $ H.div ! class_ "center" $ do
                  h1 $ a ! href "/" $ "Cubemania"
                  q "Save The World - Solve The Puzzle"
              navigation
              fromMaybe empty subnav
              H.div ! A.id "flash" ! class_ "flash notice" ! A.style "display:none" $ p mempty
              section ! A.id "content" $ H.div ! class_ "center" $ inner
              footer'
              script ! type_ "text/javascript" $ "var uvOptions = {};\n  (function() {\n    var uv = document.createElement('script'); uv.type = 'text/javascript'; uv.async = true;\n    uv.src = ('https:' == document.location.protocol ? 'https://' : 'http://') + 'widget.uservoice.com/XmjQy7dHIjHW3AR0O50Cyw.js';\n    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(uv, s);\n  })();"

withLayout :: Maybe LoggedInUser -> Page -> T.Text -> Html -> Html
withLayout currentUser currentPage title' inner = withSubnavigationLayout currentUser currentPage title' Nothing inner

usersPage :: Maybe LoggedInUser -> [SimpleUser] -> Int -> PageNumber -> Maybe T.Text -> Html
usersPage currentUser users maxSinglesCount currentPageNumber query = withLayout currentUser Users "Users" $
    H.div ! class_ "users-container" $ do
        H.form ! acceptCharset "UTF-8" ! action (toValue (usersLink Nothing)) ! A.id "users-search" ! method "get" $
            input ! A.id "q" ! required "required" ! name "q" ! placeholder "Search" ! type_ "search" ! value (toValue (fromMaybe "" query))
        ul ! A.id "users" ! class_ "users" $ forM_ users (\u -> userLi u >> space)
        unless (isJust query) $
            H.div ! class_ "pagination" $ a ! href (toValue (usersLink (Just (nextPage currentPageNumber)))) $ "Show more"
  where
    userLi user@SimpleUser{..} = li ! A.style (toValue $ "font-size: " ++ show (fontSize user maxSinglesCount) ++ "em") $ do
        a ! href (toValue $ userLink simpleUserSlug) $ toHtml simpleUserName
        space
        small ! class_ "singles" $ toHtml $ show simpleUserSinglesCount
    fontSize :: SimpleUser -> Int -> Float
    fontSize SimpleUser{..} maxSinglesCount' = fromIntegral simpleUserSinglesCount / fromIntegral maxSinglesCount' * 1.4 + 0.6

userPage :: Maybe (LoggedIn User) -> User -> Map.Map (Puzzle, Kind) (Map.Map RecordType DurationInMs) -> Maybe (Map.Map (Puzzle, Kind) (Map.Map RecordType DurationInMs)) -> Activity -> Int -> Html
userPage cu user@User{..} records ownRecords activity wastedTime = withLayout cu Users "User" $
    H.div ! A.id "user" $ do
        H.div ! class_ "admin" $ adminLink cu
        h1 $ do
            userImage Large user
            space
            toHtml userName
            small $ toHtml $ "has spent " <> humanizeTimeInterval wastedTime <> " solving puzzles."
        wcaLinkSection cu
        h3 "Activity"
        section ! A.id "activity" ! dataAttribute "activity" activityJSON $ mempty
        h3 "Records"
        ul ! class_ "records" $
            sequence_ $ Map.elems $ Map.mapWithKey (\k v -> recordWidget k v (ownRecords >>= Map.lookup k) (isJust ownRecords)) records

  where
    -- TODO: (recordRow _ _ Nothing True) makes no sense, fix the types.
    --       boolean blindness...
    recordRow :: RecordType -> Maybe DurationInMs -> Maybe DurationInMs -> Bool -> Html
    recordRow type' time ownTime comparison =
        if comparison then
            tr $ do
                th ! class_ "type" $ toHtml $ shortRecordTypeName type'
                recordComparision ownTime time
                recordEntry time
        else
            tr $ do
                th ! class_ "type" $ toHtml type'
                recordEntry time
    recordEntry :: Maybe DurationInMs -> Html
    recordEntry (Just time) =
        td $ strong $ toHtml $ Utils.formatTime time
    recordEntry Nothing =
        td $ small "None"
    recordComparision :: Maybe DurationInMs -> Maybe DurationInMs -> Html
    recordComparision (Just t1) (Just t2)
        | t1 < t2 = td ! class_ "faster" $ strong $ toHtml $ Utils.formatTime t1
        | otherwise = td ! class_ "slower" $ strong $ toHtml $ Utils.formatTime t1
    recordComparision (Just t1) Nothing =
        td ! class_ "faster" $ strong $ toHtml $ Utils.formatTime t1
    recordComparision Nothing (Just t2) =
        td ! class_ "slower" $ small "None"
    recordComparision Nothing Nothing =
        recordEntry Nothing

    recordWidget :: (Puzzle, Kind) -> Map.Map RecordType DurationInMs -> Maybe (Map.Map RecordType DurationInMs) -> Bool -> Html
    recordWidget (puzzle, kind) records ownRecords comparison =
        li ! class_ "record" $ do
            H.div ! class_ "puzzle" $ do
                H.div ! class_ (toValue $ "puzzle-image " <> posClass (puzzleCssPosition puzzle)) $ H.div ! class_ (toValue $ "kind-image " <> posClass (kindCssPosition kind)) $ mempty
                H.span $ do
                    toHtml $ fullPuzzleName (puzzle, kind)
            table $ do
                if comparison then
                    thead $ tr $ do
                        th mempty
                        th "Me"
                        th $ toHtml userName
                else
                    thead $ tr mempty
                tbody $ sequence_ $ fmap (\type' -> recordRow type' (Map.lookup type' records) (ownRecords >>= Map.lookup type') comparison) allRecordTypes
    adminLink :: Maybe LoggedInUser -> Html
    adminLink (Just (LoggedIn u))
        | u == user = a ! href (toValue $ editUserLink userSlug) $ "Edit profile"
        | otherwise = mempty
    adminLink Nothing = mempty

    posClass :: Int -> T.Text
    posClass n = "pos" <> T.pack (show n)
    wcaLinkSection :: Maybe (LoggedIn User) -> Html
    wcaLinkSection currentUser =
        case userWca of
            Just wid ->
                H.div ! A.id "wca" $ do
                    a ! href "http://www.worldcubeassociation.org" ! class_ "logo" $ mempty
                    a ! href (toValue $ wcaLink wid) $ toHtml $ userName <> "'s World Cube Association Profile"
            Nothing -> if isSelf currentUser then
                H.div ! A.id "wca" $ do
                    a ! href "http://www.worldcubeassociation.org" ! class_ "logo" $ mempty
                    a ! href (toValue $ "/users/" <> fromSlug userSlug <> "/edit") $ "Link your World Cube Association profile!"
                       else
                empty
    isSelf (Just (LoggedIn cu)) = cu == user
    isSelf _ = False

    activityJSON = toValue $ TE.decodeUtf8 $ toStrict $ JSON.encode activity

mapFromList :: Ord a => [(a, b)] -> Map.Map a [b]
mapFromList x = Map.fromListWith (++) (fmap (\(k, p) -> (k, [p])) x)

recordsPage :: Maybe LoggedInUser -> (Puzzle, Kind) -> RecordType -> [(Record, SimpleUser)] -> Int -> Int -> [(Kind, Puzzle)]-> Html
recordsPage currentUser (puzzle, kind) type' records page recordsCount foo = withSubnavigationLayout currentUser Records (fullPuzzleName (puzzle, kind) <> " Records") (Just $ puzzleNavigation (mapFromList foo) (puzzle, kind) (recordsLink Nothing Nothing)) $ do
    p ! class_ "tabs" $
        mapM_ tabEntry allRecordTypes
    table ! A.id "records" $ tbody $
        mapM_ recordEntry $ zip [((page - 1) * 50 + 1)..(page * 50)] records
    pagination
  where
    tabEntry t =
        a ! href (toValue $ recordsLink (Just t) Nothing (puzzleSlug puzzle))
          ! (if t == type' then class_ "selected" else mempty) $ do
            toHtml t
            space
    recordEntry (rank, (Record{..}, SimpleUser{..})) =
        tr ! class_ (toValue $ "record rank" <> show rank) $ do
            th $ H.span $ toHtml rank
            td $ strong $ toHtml $ Utils.formatTime recordTime
            td $ H.cite $ a ! href (toValue $ userLink simpleUserSlug) $ toHtml simpleUserName
            td $ small $ toHtml $ formatDate recordSetAt
            td $ blockquote $ toHtml recordComment
    pagination =
        H.div ! class_ "pagination" $ H.div ! class_ "pagination" $ do
            previousButton
            maybeSquashButtons firstPages
            em ! class_ "current" $ toHtml page
            maybeSquashButtons lastPages
            nextButton
    allPages = [1..lastPage]
    (firstPages, lastPages) = let (a, b) = splitAt page allPages in (init a, b)
    lastPage = recordsCount `Prelude.div` 50 + 1
    maybeSquashButtons pages =
        if length pages > 5 then do
            mapM_ middleButton $ take 2 pages
            H.span ! class_ "gap" $ "…"
            mapM_ middleButton $ drop (length pages - 2) pages
        else
            mapM_ middleButton pages
    middleButton number =
        if number == page then
            em ! class_ "current" $ toHtml number
        else
            a ! href (toValue $ recordsLink (Just type') (Just $ PageNumber number) (puzzleSlug puzzle)) $ toHtml number
    previousButton =
        if page <= 1 then
            H.span ! class_ "previous_page disabled" $ "← Previous"
        else
            a ! class_ "previous_page" ! href (toValue $ recordsLink (Just type') (Just $ PageNumber (page - 1)) (puzzleSlug puzzle)) $ "← Previous"
    nextButton =
        if page >= lastPage then
            H.span ! class_ "next_page disabled" $ "Next →"
        else
            a ! class_ "next_page" ! rel "next" ! href (toValue $ recordsLink (Just type') (Just $ PageNumber (page + 1)) (puzzleSlug puzzle)) $ "Next →"

recordShowPage :: Maybe LoggedInUser -> User -> Record -> [RecordSingle] -> (Puzzle, Kind) -> Html
recordShowPage currentUser User{..} Record{..} singles pk@(Puzzle{..}, Kind{..}) = withLayout currentUser Records "Record" $ do
    h1 $ do
        a ! href (toValue $ userLink userSlug) $
            toHtml (userName <> "'s")
        space
        (toMarkup recordType) <> " Record"
    article ! A.id "record" $ do
        header $ do
            h2 $ toHtml $ Utils.formatTime recordTime
            small $ toHtml $ Html.formatTime recordSetAt
            H.div ! class_ (toValue $ "puzzle pos" <> show puzzleCssPosition) $
                H.div ! class_ (toValue $ "kind pos" <> show kindCssPosition) $
                    toHtml $ fullPuzzleName pk
        table ! A.id "singles" $ do
            thead $ tr $ do
                th "Solve"
                th "Time"
                th "Scramble"
                th "Comment"
            tbody $ do
                sequence_ $ mapWithIndex (\s i -> singleEntry s i) singles
    bottomLine currentUser
  where
    mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
    mapWithIndex f xs = let mapWithIndex' f (x:xs) i = (f x i):(mapWithIndex' f xs (i + 1))
                            mapWithIndex' _ [] _ = []
                        in mapWithIndex' f xs 0
    singleEntry :: RecordSingle -> Int -> Html
    singleEntry RecordSingle{..} row =
        tr ! oddEvenClass row $ do
            td $ toHtml (row + 1)
            td $ strong ! timeClass recordSinglePenalty $ toHtml $ Utils.formatTime recordSingleTime
            td $ small $ toHtml recordSingleScramble
            td $ toHtml $ fromMaybe "" recordSingleComment
    timeClass (Just Dnf) = class_ "time dnf"
    timeClass _ = class_ "time"
    oddEvenClass i
      | even i = class_ "even"
      | otherwise = class_ "odd"
    bottomLine (Just (LoggedIn u)) =
        p ! class_ "suggestion" $
            a ! href (toValue $ shareRecordLink userSlug recordId) $ "Post on Facebook!"
    bottomLine Nothing =
        p ! class_ "challenge" $
            a ! href (toValue $ timerLink puzzleSlug) $ "Can you beat tim?"

rootPage :: Maybe LoggedInUser -> Maybe (Announcement, [Comment]) -> Html
rootPage currentUser post = withLayout currentUser Home "Home" $ do
    fromMaybe empty $ announcementHtml <$> post
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
    announcementHtml (Announcement{..}, comments) =
        article ! class_ "announcement" $ do
            strong $ toHtml announcementTitle
            space
            preEscapedToHtml $ renderInlineMarkdown announcementContent
            space
            a ! href (toValue $ postLinkToComments announcementId) $ toHtml $ show (length comments) <> " Comments »"

maybeUserLink :: Maybe User -> Html
maybeUserLink (Just User{..}) = a ! href (toValue $ userLink userSlug) $ toHtml userName
maybeUserLink Nothing         = "Unknown"

empty :: Html
empty = return ()

data UserImageSize = Small | Large

userImage :: UserImageSize -> User -> Html
userImage imageSize User{..} =
    let url email = "http://gravatar.com/avatar/" <> hash email <> ".png?s=" <> pxSize imageSize
        hash email = TE.decodeUtf8 $ encode $ MD5.hash (TE.encodeUtf8 email)
        pxSize Small = "25"
        pxSize Large = "60"
    in
        img ! class_ "profile-image" ! src (toValue $ url userEmail)

formatTime :: DF.FormatTime t => t -> String
formatTime = DF.formatTime DF.defaultTimeLocale "%B %d, %Y at %H:%M"

formatDate :: DF.FormatTime t => t -> String
formatDate = DF.formatTime DF.defaultTimeLocale "%B %d, %Y"

postsPage :: Maybe LoggedInUser -> [(Announcement, Maybe User, Int)] -> Html
postsPage currentUser posts = withLayout currentUser Home "Posts" $ do
    mapM_ singlePost posts
  where
    singlePost (Announcement{..}, author, commentsCount) =
        article ! class_ "post" $ do
            h1 $ a ! href (toValue $ postLink announcementId) $ toHtml announcementTitle
            section ! class_ "text" $ markdown def $ LT.fromStrict announcementContent
            H.div ! class_ "meta" $ do
                toHtml $ formatDate announcementCreatedAt
                " · "
                H.cite $ maybeUserLink author
                " · "
                a ! href (toValue $ postLinkToComments announcementId) $ toHtml $ show commentsCount <> " Comments »"

postPage :: Maybe LoggedInUser -> Announcement -> Maybe User -> [(Comment, Maybe User)] -> View T.Text -> Html
postPage currentUser Announcement{..} author comments form = withLayout currentUser Home "Post" $ do
    article ! class_ "post" $ do
        h1 $ toHtml announcementTitle
        section ! class_ "text" $
            markdown def $ LT.fromStrict announcementContent
        H.div ! class_ "meta" $ do
            toHtml $ formatDate announcementCreatedAt
            " · "
            H.cite $ maybeUserLink author
    H.div ! class_ "comments" $ do
        ol ! class_ "comments" $ mapM_ comment comments
        maybe empty (const (commentForm form)) currentUser
  where
    commentForm form = let form' = convertForm form
        in
          H.form
              ! acceptCharset "UTF-8"
              ! action (toValue $ postLinkWithComments announcementId)
              ! class_ "formtastic comment"
              ! A.id "new_comment"
              ! method "post"
              ! novalidate "novalidate" $ do
              fieldset ! class_ "inputs" $ ol $
                  textareaWithErrors "content" "Content" form' Required
              fieldset ! class_ "actions" $ ol $ li ! class_ "action input_action " ! A.id "comment_submit_action" $ input ! dataAttribute "disable-with" "Wait..." ! name "commit" ! type_ "submit" ! value "Respond"
    comment (Comment{..}, author) =
        li ! class_ "comment" ! A.id (toValue $ "comment" <> show commentId) $ do
            H.cite $ do
                maybe empty (userImage Small) author
                space
                maybeUserLink author
            small $ toHtml $ Html.formatTime commentCreatedAt
            H.div ! class_ "text" $ p $ toHtml commentContent

newPostPage :: LoggedInUser -> View T.Text -> Html
newPostPage currentUser form' = withLayout (Just currentUser) Home "New Post" $ do
    h1 "New Post"
    H.form ! method "POST"
           ! action "/posts" $ do
        fieldset ! class_ "inputs" $
            ol $ do
                textFieldWithErrors "title" "Title" (convertForm form') Required
                textareaWithErrors "content" "Content" (convertForm form') Required
        fieldset ! class_ "actions" $
            ol $
                li ! class_ "action input_action" ! A.id "post_submit_action" $
                    input ! name "commit" ! type_ "submit" ! value "Create Post"

editPostPage :: LoggedInUser -> AnnouncementId -> View T.Text -> Html
editPostPage currentUser aId form' = withLayout (Just currentUser) Home "Edit Post" $ do
    h1 "Edit Post"
    H.form ! method "POST"
           ! action (toValue $ postLink aId) $ do
        fieldset ! class_ "inputs" $
            ol $ do
                textFieldWithErrors "title" "Title" (convertForm form') Required
                textareaWithErrors "content" "Content" (convertForm form') Required
        fieldset ! class_ "actions" $
            ol $
                li ! class_ "action input_action" ! A.id "post_submit_action" $
                    input ! name "commit" ! type_ "submit" ! value "Update Post"

convertForm :: View T.Text -> View Html
convertForm = fmap H.toHtml

timerPage :: Maybe LoggedInUser -> (Puzzle, Kind) -> [(Kind, Puzzle)] -> Html
timerPage currentUser (puzzle, kind) foo = withSubnavigationLayout currentUser Timer (fullPuzzleName (puzzle, kind) <> " Timer") (Just $ puzzleNavigation (mapFromList foo) (puzzle, kind) timerLink) $ do
    H.div ! dataAttribute "puzzle" (toValue $ TE.decodeUtf8 $ toStrict $ JSON.encode $ PuzzleWithNestedKind puzzle kind)
          ! dataAttribute "user-data" (toValue $ TE.decodeUtf8 $ toStrict $ JSON.encode currentUser)
          ! A.id "backbone-container" $
        p ! class_ "suggestion" $
            "Enable JavaScript to fully enjoy Cubemania!"
    preEscapedToHtml $ backboneTemplates

space :: Html
space = toHtml (" " :: T.Text) -- Type annotation necessary for ToMarkup class

renderInlineMarkdown :: T.Text -> T.Text
renderInlineMarkdown = T.replace "</p>" "" . T.replace "<p>" "" . TE.decodeUtf8 . toStrict . renderHtml . markdown def . LT.fromStrict
