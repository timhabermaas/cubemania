{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}

module Routes
    ( CubemaniaAPI
    , api
    , usersLink
    , userLink
    , editUserLink
    , postLink
    , postLinkToComments
    , postLinkWithComments
    , wcaLink
    , recordsLink
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

type ProtectedAPI = AuthProtect "cookie-auth" :> (
                    "singles" :> ReqBody '[JSON] SubmittedSingle :> Post '[JSON] (Headers '[Header "X-NewRecord" String] Single)
                    :<|> "singles" :> Capture "singleId" SingleId :> DeleteNoContent '[JSON] NoContent
                    :<|> "singles" :> Capture "singleId" SingleId :> ReqBody '[JSON] SubmittedSingle :> Put '[JSON] NoContent
                  )

type PuzzleApi = "puzzles" :> Capture "puzzleId" PuzzleId :>
                ("singles" :> QueryParam "user_id" UserId :> QueryParam "limit" Limit :> Get '[JSON] [Single]
            :<|> "records" :> QueryParam "page" Int :> QueryParam "user_id" UserId :> Get '[JSON] [Record]
            :<|> "singles" :> "chart.json" :> QueryParam "from" Float :> QueryParam "to" Float :> QueryParam "user_id" UserId :> Get '[JSON] [ChartData]
            :<|> ProtectedAPI)
type JsonApi = "api" :> PuzzleApi
          :<|> "api" :> "users" :> QueryParam "q" T.Text :> Get '[JSON] [SimpleUser]

type UsersPath = "users" :> QueryParam "q" T.Text :> QueryParam "page" PageNumber :> Get '[HTML] Html
type UserPath = "users" :> Capture "userId" UserSlug :> Get '[HTML] Html
type PostPath = "posts" :> Capture "postId" AnnouncementId :> Get '[HTML] Html
type PostsPath = "posts" :> Get '[HTML] Html
type RecordsPath = "puzzles" :> Capture "puzzleId" PuzzleSlug :> QueryParam "type" RecordType :> QueryParam "page" PageNumber :> "records" :> Get '[HTML] Html
type TimerPath = "puzzles" :> Capture "puzzleId" PuzzleSlug :> "timer" :> Get '[HTML] Html

type RootPath = Get '[HTML] Html
type CubemaniaAPI = JsonApi
               :<|> AuthProtect "cookie-auth-optional" :> UsersPath
               :<|> AuthProtect "cookie-auth-optional" :> UserPath
               :<|> AuthProtect "cookie-auth-optional" :> PostsPath
               :<|> AuthProtect "cookie-auth-optional" :> PostPath
               :<|> AuthProtect "cookie-auth" :> "posts" :> Capture "postId" AnnouncementId :> "comments" :> ReqBody '[FormUrlEncoded] [(T.Text, T.Text)] :> Post '[HTML] Html
               :<|> AuthProtect "cookie-auth-optional" :> RecordsPath
               :<|> AuthProtect "cookie-auth-optional" :> TimerPath
               :<|> AuthProtect "cookie-auth-optional" :> RootPath

type instance AuthServerData (AuthProtect "cookie-auth") = LoggedIn User
type instance AuthServerData (AuthProtect "cookie-auth-optional") = Maybe (LoggedIn User)

api :: Proxy CubemaniaAPI
api = Proxy

--linkTo :: Proxy UsersPath -> MkLink UsersPath
--linkTo = safeLink api

usersLink :: Maybe PageNumber -> T.Text
--usersLink page = "/" `T.append` (T.pack . show $ linkTo (Proxy :: Proxy UsersPath) Nothing page)
usersLink page = "/users" <> (maybe "" (\page -> "?page=" <> (T.pack $ show $ fromPageNumber page)) page)

userLink :: UserSlug -> T.Text
userLink (UserSlug slug) = "/users/" <> slug

editUserLink :: UserSlug -> T.Text
editUserLink (UserSlug slug) = "/users/" <> slug <> "/edit"


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
    queryPart (Just type') (Just p) = "?type=" <> toQueryParam type' <> "&page=" <> toQueryParam p
    queryPart Nothing (Just p) = "?page=" <> toQueryParam p
    queryPart (Just type') Nothing = "?type=" <> toQueryParam type'
    queryPart Nothing Nothing = ""
