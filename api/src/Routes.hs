{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Routes
    ( CubemaniaRoutes
    , api
    , usersLink
    , userLink
    , editUserLink
    , deleteUserLink
    , postLink
    , postLinkToComments
    , postLinkWithComments
    , wcaLink
    , recordsLink
    , recordLink
    , facebookShareLink
    , shareRecordLink
    , timerLink
    ) where

import Types

import Prelude hiding (id)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Servant
import Servant.Server.Experimental.Auth (AuthServerData)
import Text.Blaze.Html5 (Html)
import MyServantBlaze
import Utils
import qualified Network.HTTP.Types.URI as URI
import qualified Data.Text.Encoding as TE
import Data.ByteString.Char8 (pack)

type ProtectedAPI = AuthProtect "cookie-auth" :> (
                    "singles" :> ReqBody '[JSON] SubmittedSingle :> Post '[JSON] (Headers '[Header "X-NewRecord" String] Single)
                    :<|> "singles" :> Capture "singleId" SingleId :> DeleteNoContent '[JSON] NoContent
                    :<|> "singles" :> Capture "singleId" SingleId :> ReqBody '[JSON] SubmittedSingle :> Put '[JSON] NoContent
                  )

type PuzzleApi = "puzzles" :> Capture "puzzleId" PuzzleId :>
                ("singles" :> QueryParam "user_id" UserId :> QueryParam "limit" Limit :> Get '[JSON] [Single]
            :<|> "records" :> QueryParam "page" Int :> QueryParam "user_id" UserId :> Get '[JSON] [RecordWithSingles]
            :<|> "singles" :> "chart.json" :> QueryParam "from" Float :> QueryParam "to" Float :> QueryParam "user_id" UserId :> Get '[JSON] [ChartData]
            :<|> ProtectedAPI)
type JsonApi = "api" :> PuzzleApi
          :<|> "api" :> "users" :> QueryParam "q" T.Text :> Get '[JSON] [SimpleUser]

type UsersPath = "users" :> QueryParam "q" T.Text :> QueryParam "page" PageNumber :> Get '[HTML] Html
type UserPath = Header "Cookie" T.Text :> "users" :> Capture "userId" UserSlug :> Get '[HTML] (Headers '[Header "Set-Cookie" T.Text] Html)
type EditUserPath = "users" :> Capture "userId" UserSlug :> "edit" :> Get '[HTML] Html
type UpdateUserPath = "users" :> Capture "userId" UserSlug :> ReqBody '[FormUrlEncoded] [(T.Text, T.Text)] :> Post '[HTML] Html
type DeleteUserPath = "users" :> Capture "userId" UserSlug :> "delete" :> Post '[HTML] Html
type PostPath = "posts" :> Capture "postId" AnnouncementId :> Get '[HTML] Html
type NewPostPath = "posts" :> "new" :> Get '[HTML] Html
type EditPostPath = "posts" :> Capture "postId" AnnouncementId :> "edit" :> Get '[HTML] Html
type UpdatePostPath = "posts" :> Capture "postId" AnnouncementId :> ReqBody '[FormUrlEncoded] [(T.Text, T.Text)] :> Post '[HTML] Html
type CreateCommentPath = "posts" :> Capture "postId" AnnouncementId :> "comments" :> ReqBody '[FormUrlEncoded] [(T.Text, T.Text)] :> Post '[HTML] Html
type CreatePostPath = "posts" :> ReqBody '[FormUrlEncoded] [(T.Text, T.Text)] :> Post '[HTML] Html
type PostsPath = "posts" :> Get '[HTML] Html
type RecordPath = "users" :> Capture "userSlug" UserSlug :> "records" :> Capture "recordId" (Id Record) :> Get '[HTML] Html
type ShareRecordPath = "users" :> Capture "userSlug" UserSlug :> "records" :> Capture "recordId" (Id Record) :> "share" :> Get '[HTML] Html
type RecordsPath = "puzzles" :> Capture "puzzleId" PuzzleSlug :> QueryParam "type" RecordType :> QueryParam "page" PageNumber :> "records" :> Get '[HTML] Html
type TimerPath = "puzzles" :> Capture "puzzleId" PuzzleSlug :> "timer" :> Get '[HTML] Html
type GetRegisterPath = "register" :> Get '[HTML] Html
type RegisterPath = "users" :> ReqBody '[FormUrlEncoded] [(T.Text, T.Text)] :> Post '[HTML] Html
type GetLoginPath = "login" :> Get '[HTML] Html
type LoginPath = "session" :> ReqBody '[FormUrlEncoded] [(T.Text, T.Text)] :> Post '[HTML] Html--(Headers '[Header "Set-Cookie" T.Text] Html)
-- TODO: Replace with POST
type LogoutPath = "logout" :> Get '[HTML] Html
type GetResetPasswordPath = "reset_password" :> "new" :> Get '[HTML] Html
type ResetPasswordPath = "reset_password" :> ReqBody '[FormUrlEncoded] [(T.Text, T.Text)] :> Post '[HTML] Html

type RootPath = Header "Cookie" T.Text :> Get '[HTML] (Headers '[Header "Set-Cookie" T.Text] Html)
type CubemaniaRoutes
    = JsonApi
 :<|> AuthProtect "flash-message" :> (
 AuthProtect "cookie-auth-optional" :> UsersPath
 :<|> AuthProtect "cookie-auth-optional" :> UserPath
 :<|> AuthProtect "cookie-auth-optional" :> PostsPath
 :<|> AuthProtect "cookie-auth-optional" :> PostPath
 :<|> AuthProtect "cookie-auth" :> NewPostPath
 :<|> AuthProtect "cookie-auth" :> CreatePostPath
 :<|> AuthProtect "cookie-auth" :> EditPostPath
 :<|> AuthProtect "cookie-auth" :> UpdatePostPath
 :<|> AuthProtect "cookie-auth" :> CreateCommentPath
 :<|> AuthProtect "cookie-auth" :> EditUserPath
 :<|> AuthProtect "cookie-auth" :> UpdateUserPath
 :<|> AuthProtect "cookie-auth" :> DeleteUserPath
 :<|> AuthProtect "cookie-auth-optional" :> RecordsPath
 :<|> AuthProtect "cookie-auth-optional" :> RecordPath
 :<|> AuthProtect "cookie-auth" :> ShareRecordPath
 :<|> AuthProtect "cookie-auth-optional" :> TimerPath
 :<|> AuthProtect "cookie-auth-optional" :> GetRegisterPath
 :<|> AuthProtect "cookie-auth-optional" :> RegisterPath
 :<|> AuthProtect "cookie-auth-optional" :> GetLoginPath
 :<|> AuthProtect "cookie-auth-optional" :> LoginPath
 :<|> AuthProtect "cookie-auth" :> LogoutPath
 :<|> AuthProtect "cookie-auth-optional" :> GetResetPasswordPath
 :<|> AuthProtect "cookie-auth-optional" :> ResetPasswordPath
 :<|> AuthProtect "cookie-auth-optional" :> RootPath)

type instance AuthServerData (AuthProtect "cookie-auth") = LoggedIn User
type instance AuthServerData (AuthProtect "cookie-auth-optional") = Maybe (LoggedIn User)
type instance AuthServerData (AuthProtect "flash-message") = Maybe FlashMessage

api :: Proxy CubemaniaRoutes
api = Proxy

usersLink :: Maybe PageNumber -> T.Text
usersLink page = "/users" <> (maybe "" (\p -> "?page=" <> (T.pack $ show $ fromPageNumber p)) page)

userLink :: UserSlug -> T.Text
userLink (UserSlug slug) = "/users/" <> slug

editUserLink :: UserSlug -> T.Text
editUserLink (UserSlug slug) = "/users/" <> slug <> "/edit"

deleteUserLink :: UserSlug -> T.Text
deleteUserLink (UserSlug slug) = "/users/" <> slug <> "/delete"


postLinkToComments :: AnnouncementId -> T.Text
postLinkToComments aId = postLink aId <> "#comments"

postLink :: AnnouncementId -> T.Text
postLink aId = "/posts/" <> T.pack (show aId)

postLinkWithComments :: AnnouncementId -> T.Text
postLinkWithComments aId = postLink aId <> "/comments"

wcaLink :: T.Text -> T.Text
wcaLink id = "http://www.worldcubeassociation.org/results/p.php?i=" <> id

timerLink :: PuzzleSlug -> T.Text
timerLink (PuzzleSlug slug) = "/puzzles/" <> slug <> "/timer"

recordsLink :: Maybe RecordType -> Maybe PageNumber -> PuzzleSlug -> T.Text
recordsLink type' page (PuzzleSlug slug) =
    "/puzzles/" <> slug <> "/records" <> queryPart type' page
  where
    queryPart (Just t) (Just p) = "?type=" <> toQueryParam t <> "&page=" <> toQueryParam p
    queryPart Nothing (Just p) = "?page=" <> toQueryParam p
    queryPart (Just t) Nothing = "?type=" <> toQueryParam t
    queryPart Nothing Nothing = ""

recordLink :: UserSlug -> Id Record -> T.Text
recordLink (UserSlug userSlug) (Id recordId) = "/users/" <> userSlug <> "/records/" <> T.pack (show recordId)

shareRecordLink :: UserSlug -> Id Record -> T.Text
shareRecordLink (UserSlug u) (Id rId) = "/users/" <> u <> "/records/" <> T.pack (show rId) <> "/share"

facebookShareLink :: String -> User -> (DbEntry Record, [Single]) -> (Puzzle, Kind) -> T.Text
facebookShareLink appId User{..} (DbEntry recordId Record{..}, singles) (puzzle, kind) =
    "http://www.facebook.com/dialog/feed" <> TE.decodeUtf8 (URI.renderSimpleQuery True facebookParams)
  where
    facebookParams =
        [ ("app_id", pack appId)
        , ("link", TE.encodeUtf8 ("https://www.cubemania.org" <> recordLink userSlug recordId))
        , ("name", TE.encodeUtf8 name)
        , ("caption", "Keep track of your times and join Cubemania!")
        , ("description", TE.encodeUtf8 $ description recordType)
        , ("redirect_uri", TE.encodeUtf8 ("https://www.cubemania.org" <> recordLink userSlug recordId))
        ]
    name = userName <> "has a new " <> fullPuzzleName (puzzle, kind) <> " record: " <> formatTime recordTime
    description SingleRecord = singleScramble $ singles !! 0
    description _ = T.intercalate " " $ fmap (\s -> if minimum singles == s || maximum singles == s then "(" <> formatTimeWithDnf s <> ")" else formatTimeWithDnf s) singles
    formatTimeWithDnf s
        | singlePenalty s == Just Dnf = "DNF"
        | otherwise = Utils.formatTime $ singleTime s
