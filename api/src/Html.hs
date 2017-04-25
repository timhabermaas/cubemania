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
    , registerPage
    , loginPage
    , Page
    ) where

import Data.Monoid ((<>))
import Types
import Routes
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map.Strict as Map
import Data.ByteString.Lazy (toStrict)
import Control.Monad (forM_, unless)
import Control.Monad.Reader (Reader, ask)
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
import Utils
import Frontend.FormViewHelpers
import Frontend.PuzzleNavigation
import Frontend.BackboneTemplates

data NavigationPage = Home | Timer | Users | Records deriving (Eq)

navigationItems :: [NavigationPage]
navigationItems = [Home, Timer, Users, Records]

navigationText :: NavigationPage -> T.Text
navigationText Home = "Home"
navigationText Timer = "Timer"
navigationText Users = "Users"
navigationText Records = "Records"

navigationLink :: NavigationPage -> T.Text
navigationLink Home = "/"
navigationLink Timer = "/puzzles/3x3x3/timer"
navigationLink Users = usersLink Nothing
navigationLink Records = "/puzzles/3x3x3/records"

type Page = Reader (Maybe FlashMessage) Html

withSubnavigationLayout :: Maybe LoggedInUser -> NavigationPage -> T.Text -> Maybe Html -> (Maybe FlashMessage) -> Html -> Html
withSubnavigationLayout currentUser currentPage title' subnav flash inner =
    let
      selectedClass item' = if item' == currentPage then "selected" else ""
      navigationItem item' = li ! class_ (toValue (selectedClass item' :: T.Text)) $
                                 a ! href (toValue $ navigationLink item') $
                                     toHtml $ navigationText item'
      sessionNavigation =
          case currentUser of
              Just (LoggedIn u _) -> do
                  li ! class_ "session" $ a ! href "/logout" $ "Logout"
                  li ! class_ "session" $ a ! href (toValue $ userLink (userSlug u)) $ toHtml (userName u <> "'s Profile")
              Nothing ->
                  li ! class_ "session" $ a ! href "/login" $ "Login"
      navigation = nav ! class_ "main" $ ul $ mapM_ navigationItem navigationItems <> sessionNavigation
      flashMessage message =
          H.div ! A.id "flash" ! class_ "flash notice" $ p (toHtml message)
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
              --flashMessage "foo"
              maybe empty flashMessage flash
              section ! A.id "content" $ H.div ! class_ "center" $ inner
              footer'
              script ! type_ "text/javascript" $ "var uvOptions = {};\n  (function() {\n    var uv = document.createElement('script'); uv.type = 'text/javascript'; uv.async = true;\n    uv.src = ('https:' == document.location.protocol ? 'https://' : 'http://') + 'widget.uservoice.com/XmjQy7dHIjHW3AR0O50Cyw.js';\n    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(uv, s);\n  })();"

withLayout :: Maybe LoggedInUser -> NavigationPage -> T.Text -> (Maybe FlashMessage) -> Html -> Html
withLayout currentUser currentPage title' flash inner = withSubnavigationLayout currentUser currentPage title' Nothing flash inner

usersPage :: Maybe LoggedInUser -> [SimpleUser] -> Int -> PageNumber -> Maybe T.Text -> Html
usersPage currentUser users maxSinglesCount currentPageNumber query = withLayout currentUser Users "Users" Nothing $
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
userPage cu user@User{..} records ownRecords activity wastedTime = withLayout cu Users "User" Nothing $
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
    recordComparision Nothing (Just _) =
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
    adminLink (Just (LoggedIn u _))
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
    isSelf (Just (LoggedIn cu _)) = cu == user
    isSelf _ = False

    activityJSON = toValue $ TE.decodeUtf8 $ toStrict $ JSON.encode activity

mapFromList :: Ord a => [(a, b)] -> Map.Map a [b]
mapFromList x = Map.fromListWith (++) (fmap (\(k, p) -> (k, [p])) x)

recordsPage :: Maybe LoggedInUser -> (Puzzle, Kind) -> RecordType -> [(Record, SimpleUser)] -> Int -> Int -> [(Kind, Puzzle)]-> Html
recordsPage currentUser (puzzle, kind) type' records page recordsCount foo = withSubnavigationLayout currentUser Records (fullPuzzleName (puzzle, kind) <> " Records") (Just $ puzzleNavigation (mapFromList foo) (puzzle, kind) (recordsLink Nothing Nothing)) Nothing $ do
    p ! class_ "tabs" $
        mapM_ tabEntry allRecordTypes
    table ! A.id "records" $ tbody $
        mapM_ recordEntry $ zip [((page - 1) * 50 + 1)..(page * 50)] records
    pagination
  where
    tabEntry t = do
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

recordShowPage :: Maybe LoggedInUser -> User -> Record -> [Single] -> (Puzzle, Kind) -> Html
recordShowPage currentUser User{..} Record{..} singles pk@(Puzzle{..}, Kind{..}) = withLayout currentUser Records "Record" Nothing $ do
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
    singleEntry :: Single -> Int -> Html
    singleEntry Single{..} row =
        tr ! oddEvenClass row $ do
            td $ toHtml (row + 1)
            td $ strong ! timeClass singlePenalty $ toHtml $ Utils.formatTime singleTime
            td $ small $ toHtml singleScramble
            td $ toHtml $ fromMaybe "" singleComment
    timeClass (Just Dnf) = class_ "time dnf"
    timeClass _ = class_ "time"
    oddEvenClass i
      | even i = class_ "even"
      | otherwise = class_ "odd"
    bottomLine (Just (LoggedIn u _)) =
        p ! class_ "suggestion" $
            a ! href (toValue $ shareRecordLink userSlug recordId) $ "Post on Facebook!"
    bottomLine Nothing =
        p ! class_ "challenge" $
            a ! href (toValue $ timerLink puzzleSlug) $ "Can you beat tim?"

rootPage :: Maybe LoggedInUser -> Maybe (Announcement, [Comment]) -> Page-- Html
rootPage currentUser post = do
    flash <- ask
    return $ withLayout currentUser Home "Home" flash $ do
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
    let url email = "http://gravatar.com/avatar/" <> gravatarHash email <> ".png?s=" <> pxSize imageSize
        pxSize Small = "25"
        pxSize Large = "60"
    in
        img ! class_ "profile-image" ! src (toValue $ url userEmail)

formatTime :: DF.FormatTime t => t -> String
formatTime = DF.formatTime DF.defaultTimeLocale "%B %d, %Y at %H:%M"

formatDate :: DF.FormatTime t => t -> String
formatDate = DF.formatTime DF.defaultTimeLocale "%B %d, %Y"

postsPage :: Maybe LoggedInUser -> [(Announcement, Maybe User, Int)] -> Html
postsPage currentUser posts = withLayout currentUser Home "Posts" Nothing $ do
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
postPage currentUser Announcement{..} author comments form = withLayout currentUser Home "Post" Nothing $ do
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
              fieldset ! class_ "actions" $ ol $ li ! class_ "action input_action " ! A.id "comment_submit_action" $
                  input ! dataAttribute "disable-with" "Wait..." ! name "commit" ! type_ "submit" ! value "Respond"
    comment (Comment{..}, author) =
        li ! class_ "comment" ! A.id (toValue $ "comment" <> show commentId) $ do
            H.cite $ do
                maybe empty (userImage Small) author
                space
                maybeUserLink author
            small $ toHtml $ Html.formatTime commentCreatedAt
            H.div ! class_ "text" $ p $ toHtml commentContent

newPostPage :: LoggedInUser -> View T.Text -> Html
newPostPage currentUser form' = withLayout (Just currentUser) Home "New Post" Nothing $ do
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
editPostPage currentUser aId form' = withLayout (Just currentUser) Home "Edit Post" Nothing $ do
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
timerPage currentUser (puzzle, kind) foo = withSubnavigationLayout currentUser Timer (fullPuzzleName (puzzle, kind) <> " Timer") (Just $ puzzleNavigation (mapFromList foo) (puzzle, kind) timerLink) Nothing $ do
    H.div ! dataAttribute "puzzle" (toValue $ TE.decodeUtf8 $ toStrict $ JSON.encode $ PuzzleWithNestedKind puzzle kind)
          ! dataAttribute "user-data" (toValue $ TE.decodeUtf8 $ toStrict $ JSON.encode $ getLoggedIn <$> currentUser)
          ! A.id "backbone-container" $
        p ! class_ "suggestion" $
            "Enable JavaScript to fully enjoy Cubemania!"
    preEscapedToHtml $ backboneTemplates

registerPage :: Maybe LoggedInUser -> View T.Text -> Html
registerPage currentUser form' = withLayout currentUser Users "Register" Nothing $ do
    h1 "Register"
    H.form ! acceptCharset "UTF-8" ! action "/users" ! class_ "formtastic user" ! A.id "new_user" ! method "post" ! novalidate "novalidate" $ do
        fieldset ! class_ "inputs" $ ol $ do
            textFieldWithErrors "bot" "Bot" (convertForm form') Required
            textFieldWithErrors "name" "Name" (convertForm form') Required
            textFieldWithErrors "email" "Email" (convertForm form') Required
            textFieldWithErrors "wcaId" "WCA ID (optional)" (convertForm form') Optional
            passwordFieldWithErrors "password.p1" "Password" (convertForm form')
            passwordFieldWithDifferentErrors "password.p2" ["password", "password.p2"] "Confirmation" (convertForm form')
            H.div ! class_ "bot" $ li ! class_ "string input optional stringish" ! A.id "user_bot_email_input" $ do
                H.label ! class_ " label" ! for "user_bot_email" $ "Bot email"
                input ! A.id "user_bot_email" ! name "user[bot_email]" ! type_ "text"
            li ! class_ "time_zone input optional" ! A.id "user_time_zone_input" $ do
                H.label ! class_ " label" ! for "user_time_zone" $ "Time zone"
                select ! A.id "user_time_zone" ! name "user[time_zone]" $ do
                    option ! value "" ! disabled "disabled" $ "-------------"
                    option ! value "American Samoa" $ "(GMT-11:00) American Samoa"
                    option ! value "International Date Line West" $ "(GMT-11:00) International Date Line West"
                    option ! value "Midway Island" $ "(GMT-11:00) Midway Island"
                    option ! value "Hawaii" $ "(GMT-10:00) Hawaii"
                    option ! value "Alaska" $ "(GMT-09:00) Alaska"
                    option ! value "Pacific Time (US & Canada)" $ "(GMT-08:00) Pacific Time (US & Canada)"
                    option ! value "Tijuana" $ "(GMT-08:00) Tijuana"
                    option ! value "Arizona" $ "(GMT-07:00) Arizona"
                    option ! value "Chihuahua" $ "(GMT-07:00) Chihuahua"
                    option ! value "Mazatlan" $ "(GMT-07:00) Mazatlan"
                    option ! value "Mountain Time (US & Canada)" $ "(GMT-07:00) Mountain Time (US & Canada)"
                    option ! value "Central America" $ "(GMT-06:00) Central America"
                    option ! value "Central Time (US & Canada)" $ "(GMT-06:00) Central Time (US & Canada)"
                    option ! value "Guadalajara" $ "(GMT-06:00) Guadalajara"
                    option ! value "Mexico City" $ "(GMT-06:00) Mexico City"
                    option ! value "Monterrey" $ "(GMT-06:00) Monterrey"
                    option ! value "Saskatchewan" $ "(GMT-06:00) Saskatchewan"
                    option ! value "Bogota" $ "(GMT-05:00) Bogota"
                    option ! value "Eastern Time (US & Canada)" $ "(GMT-05:00) Eastern Time (US & Canada)"
                    option ! value "Indiana (East)" $ "(GMT-05:00) Indiana (East)"
                    option ! value "Lima" $ "(GMT-05:00) Lima"
                    option ! value "Quito" $ "(GMT-05:00) Quito"
                    option ! value "Caracas" $ "(GMT-04:30) Caracas"
                    option ! value "Atlantic Time (Canada)" $ "(GMT-04:00) Atlantic Time (Canada)"
                    option ! value "Georgetown" $ "(GMT-04:00) Georgetown"
                    option ! value "La Paz" $ "(GMT-04:00) La Paz"
                    option ! value "Newfoundland" $ "(GMT-03:30) Newfoundland"
                    option ! value "Brasilia" $ "(GMT-03:00) Brasilia"
                    option ! value "Buenos Aires" $ "(GMT-03:00) Buenos Aires"
                    option ! value "Greenland" $ "(GMT-03:00) Greenland"
                    option ! value "Santiago" $ "(GMT-03:00) Santiago"
                    option ! value "Mid-Atlantic" $ "(GMT-02:00) Mid-Atlantic"
                    option ! value "Azores" $ "(GMT-01:00) Azores"
                    option ! value "Cape Verde Is." $ "(GMT-01:00) Cape Verde Is."
                    option ! value "Casablanca" $ "(GMT+00:00) Casablanca"
                    option ! value "Dublin" $ "(GMT+00:00) Dublin"
                    option ! value "Edinburgh" $ "(GMT+00:00) Edinburgh"
                    option ! value "Lisbon" $ "(GMT+00:00) Lisbon"
                    option ! value "London" $ "(GMT+00:00) London"
                    option ! value "Monrovia" $ "(GMT+00:00) Monrovia"
                    option ! value "UTC" ! selected "selected" $ "(GMT+00:00) UTC"
                    option ! value "Amsterdam" $ "(GMT+01:00) Amsterdam"
                    option ! value "Belgrade" $ "(GMT+01:00) Belgrade"
                    option ! value "Berlin" $ "(GMT+01:00) Berlin"
                    option ! value "Bern" $ "(GMT+01:00) Bern"
                    option ! value "Bratislava" $ "(GMT+01:00) Bratislava"
                    option ! value "Brussels" $ "(GMT+01:00) Brussels"
                    option ! value "Budapest" $ "(GMT+01:00) Budapest"
                    option ! value "Copenhagen" $ "(GMT+01:00) Copenhagen"
                    option ! value "Ljubljana" $ "(GMT+01:00) Ljubljana"
                    option ! value "Madrid" $ "(GMT+01:00) Madrid"
                    option ! value "Paris" $ "(GMT+01:00) Paris"
                    option ! value "Prague" $ "(GMT+01:00) Prague"
                    option ! value "Rome" $ "(GMT+01:00) Rome"
                    option ! value "Sarajevo" $ "(GMT+01:00) Sarajevo"
                    option ! value "Skopje" $ "(GMT+01:00) Skopje"
                    option ! value "Stockholm" $ "(GMT+01:00) Stockholm"
                    option ! value "Vienna" $ "(GMT+01:00) Vienna"
                    option ! value "Warsaw" $ "(GMT+01:00) Warsaw"
                    option ! value "West Central Africa" $ "(GMT+01:00) West Central Africa"
                    option ! value "Zagreb" $ "(GMT+01:00) Zagreb"
                    option ! value "Athens" $ "(GMT+02:00) Athens"
                    option ! value "Bucharest" $ "(GMT+02:00) Bucharest"
                    option ! value "Cairo" $ "(GMT+02:00) Cairo"
                    option ! value "Harare" $ "(GMT+02:00) Harare"
                    option ! value "Helsinki" $ "(GMT+02:00) Helsinki"
                    option ! value "Istanbul" $ "(GMT+02:00) Istanbul"
                    option ! value "Jerusalem" $ "(GMT+02:00) Jerusalem"
                    option ! value "Kyiv" $ "(GMT+02:00) Kyiv"
                    option ! value "Pretoria" $ "(GMT+02:00) Pretoria"
                    option ! value "Riga" $ "(GMT+02:00) Riga"
                    option ! value "Sofia" $ "(GMT+02:00) Sofia"
                    option ! value "Tallinn" $ "(GMT+02:00) Tallinn"
                    option ! value "Vilnius" $ "(GMT+02:00) Vilnius"
                    option ! value "Baghdad" $ "(GMT+03:00) Baghdad"
                    option ! value "Kuwait" $ "(GMT+03:00) Kuwait"
                    option ! value "Minsk" $ "(GMT+03:00) Minsk"
                    option ! value "Moscow" $ "(GMT+03:00) Moscow"
                    option ! value "Nairobi" $ "(GMT+03:00) Nairobi"
                    option ! value "Riyadh" $ "(GMT+03:00) Riyadh"
                    option ! value "St. Petersburg" $ "(GMT+03:00) St. Petersburg"
                    option ! value "Volgograd" $ "(GMT+03:00) Volgograd"
                    option ! value "Tehran" $ "(GMT+03:30) Tehran"
                    option ! value "Abu Dhabi" $ "(GMT+04:00) Abu Dhabi"
                    option ! value "Baku" $ "(GMT+04:00) Baku"
                    option ! value "Muscat" $ "(GMT+04:00) Muscat"
                    option ! value "Tbilisi" $ "(GMT+04:00) Tbilisi"
                    option ! value "Yerevan" $ "(GMT+04:00) Yerevan"
                    option ! value "Kabul" $ "(GMT+04:30) Kabul"
                    option ! value "Ekaterinburg" $ "(GMT+05:00) Ekaterinburg"
                    option ! value "Islamabad" $ "(GMT+05:00) Islamabad"
                    option ! value "Karachi" $ "(GMT+05:00) Karachi"
                    option ! value "Tashkent" $ "(GMT+05:00) Tashkent"
                    option ! value "Chennai" $ "(GMT+05:30) Chennai"
                    option ! value "Kolkata" $ "(GMT+05:30) Kolkata"
                    option ! value "Mumbai" $ "(GMT+05:30) Mumbai"
                    option ! value "New Delhi" $ "(GMT+05:30) New Delhi"
                    option ! value "Sri Jayawardenepura" $ "(GMT+05:30) Sri Jayawardenepura"
                    option ! value "Kathmandu" $ "(GMT+05:45) Kathmandu"
                    option ! value "Almaty" $ "(GMT+06:00) Almaty"
                    option ! value "Astana" $ "(GMT+06:00) Astana"
                    option ! value "Dhaka" $ "(GMT+06:00) Dhaka"
                    option ! value "Novosibirsk" $ "(GMT+06:00) Novosibirsk"
                    option ! value "Urumqi" $ "(GMT+06:00) Urumqi"
                    option ! value "Rangoon" $ "(GMT+06:30) Rangoon"
                    option ! value "Bangkok" $ "(GMT+07:00) Bangkok"
                    option ! value "Hanoi" $ "(GMT+07:00) Hanoi"
                    option ! value "Jakarta" $ "(GMT+07:00) Jakarta"
                    option ! value "Krasnoyarsk" $ "(GMT+07:00) Krasnoyarsk"
                    option ! value "Beijing" $ "(GMT+08:00) Beijing"
                    option ! value "Chongqing" $ "(GMT+08:00) Chongqing"
                    option ! value "Hong Kong" $ "(GMT+08:00) Hong Kong"
                    option ! value "Irkutsk" $ "(GMT+08:00) Irkutsk"
                    option ! value "Kuala Lumpur" $ "(GMT+08:00) Kuala Lumpur"
                    option ! value "Perth" $ "(GMT+08:00) Perth"
                    option ! value "Singapore" $ "(GMT+08:00) Singapore"
                    option ! value "Taipei" $ "(GMT+08:00) Taipei"
                    option ! value "Ulaan Bataar" $ "(GMT+08:00) Ulaan Bataar"
                    option ! value "Osaka" $ "(GMT+09:00) Osaka"
                    option ! value "Sapporo" $ "(GMT+09:00) Sapporo"
                    option ! value "Seoul" $ "(GMT+09:00) Seoul"
                    option ! value "Tokyo" $ "(GMT+09:00) Tokyo"
                    option ! value "Yakutsk" $ "(GMT+09:00) Yakutsk"
                    option ! value "Adelaide" $ "(GMT+09:30) Adelaide"
                    option ! value "Darwin" $ "(GMT+09:30) Darwin"
                    option ! value "Brisbane" $ "(GMT+10:00) Brisbane"
                    option ! value "Canberra" $ "(GMT+10:00) Canberra"
                    option ! value "Guam" $ "(GMT+10:00) Guam"
                    option ! value "Hobart" $ "(GMT+10:00) Hobart"
                    option ! value "Magadan" $ "(GMT+10:00) Magadan"
                    option ! value "Melbourne" $ "(GMT+10:00) Melbourne"
                    option ! value "Port Moresby" $ "(GMT+10:00) Port Moresby"
                    option ! value "Solomon Is." $ "(GMT+10:00) Solomon Is."
                    option ! value "Sydney" $ "(GMT+10:00) Sydney"
                    option ! value "Vladivostok" $ "(GMT+10:00) Vladivostok"
                    option ! value "New Caledonia" $ "(GMT+11:00) New Caledonia"
                    option ! value "Auckland" $ "(GMT+12:00) Auckland"
                    option ! value "Fiji" $ "(GMT+12:00) Fiji"
                    option ! value "Kamchatka" $ "(GMT+12:00) Kamchatka"
                    option ! value "Marshall Is." $ "(GMT+12:00) Marshall Is."
                    option ! value "Wellington" $ "(GMT+12:00) Wellington"
                    option ! value "Nuku'alofa" $ "(GMT+13:00) Nuku'alofa"
                    option ! value "Samoa" $ "(GMT+13:00) Samoa"
                    option ! value "Tokelau Is." $ "(GMT+13:00) Tokelau Is."
        fieldset ! class_ "actions" $ ol $ li ! class_ "action input_action " ! A.id "user_submit_action" $ input ! name "commit" ! type_ "submit" ! value "Register"

loginPage :: Maybe LoggedInUser -> View T.Text -> Page
loginPage currentUser form' = do
    flash <- ask
    return $ withLayout currentUser Users "Login" flash $ do
        h1 "Login"
        H.form ! acceptCharset "UTF-8" ! action "/session" ! class_ "formtastic login" ! A.id "new_login" ! method "post" ! novalidate "novalidate" $ do
            fieldset ! class_ "inputs" $ ol $ do
                textFieldWithErrors "name" "Name" (convertForm form') Required
                passwordFieldWithErrors "password" "Password" (convertForm form')
            fieldset ! class_ "actions" $ ol $ li ! class_ "action input_action " ! A.id "login_submit_action" $ input ! name "commit" ! type_ "submit" ! value "Login"
        a ! href "/register" $ "Register"
        space
        "·"
        space
        a ! href "/reset_password/new" $ "Forgot your password?"


space :: Html
space = toHtml (" " :: T.Text) -- Type annotation necessary for ToMarkup class

renderInlineMarkdown :: T.Text -> T.Text
renderInlineMarkdown = T.replace "</p>" "" . T.replace "<p>" "" . TE.decodeUtf8 . toStrict . renderHtml . markdown def . LT.fromStrict
