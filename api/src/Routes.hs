{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings #-}

module Routes
    ( CubemaniaAPI
    , api
    , usersLink
    , userLink
    , postLink
    , postLinkToComments
    , wcaLink
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
type RootPath = Get '[HTML] Html
type CubemaniaAPI = JsonApi
               :<|> AuthProtect "cookie-auth-optional" :> UsersPath
               :<|> AuthProtect "cookie-auth-optional" :> UserPath
               :<|> AuthProtect "cookie-auth-optional" :> RootPath

type instance AuthServerData (AuthProtect "cookie-auth") = UserId
type instance AuthServerData (AuthProtect "cookie-auth-optional") = Maybe UserId

api :: Proxy CubemaniaAPI
api = Proxy

--linkTo :: Proxy UsersPath -> MkLink UsersPath
--linkTo = safeLink api

usersLink :: Maybe PageNumber -> T.Text
--usersLink page = "/" `T.append` (T.pack . show $ linkTo (Proxy :: Proxy UsersPath) Nothing page)
usersLink page = "/users" <> (maybe "" (\page -> "?page=" <> (T.pack $ show $ fromPageNumber page)) page)

userLink :: UserSlug -> T.Text
userLink (UserSlug slug) = "/users/" <> slug

postLinkToComments :: AnnouncementId -> T.Text
postLinkToComments aId = postLink aId <> "#comments"

postLink :: AnnouncementId -> T.Text
postLink aId = "/posts/" <> T.pack (show aId)

wcaLink :: T.Text -> T.Text
wcaLink id = "http://www.worldcubeassociation.org/results/p.php?i=" <> id
