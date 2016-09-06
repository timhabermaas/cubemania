{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings #-}

module Routes
    ( CubemaniaAPI
    , api
    , usersLink
    , postLink
    , postLinkToComments
    ) where

import Types

import Data.Monoid ((<>))
import qualified Data.Text as T
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler, AuthServerData)
import Servant.Utils.Links (safeLink)
import Text.Blaze.Html5 (Html)
import MyServantBlaze

type ProtectedAPI = AuthProtect "cookie-auth" :> (
                    "singles" :> ReqBody '[JSON] SubmittedSingle :> Post '[JSON] (Headers '[Header "X-NewRecord" String] Single)
                    :<|> "singles" :> Capture "singleId" SingleId :> DeleteNoContent '[JSON] NoContent
                    :<|> "singles" :> Capture "singleId" SingleId :> ReqBody '[JSON] SubmittedSingle :> Put '[JSON] NoContent
                  )

type PuzzleAPI = "api" :> "puzzles" :> Capture "puzzleId" PuzzleId :>
                      ("singles" :> QueryParam "user_id" UserId :> QueryParam "limit" Limit :> Get '[JSON] [Single]
                  :<|> "records" :> QueryParam "page" Int :> QueryParam "user_id" UserId :> Get '[JSON] [Record]
                  :<|> "singles" :> "chart.json" :> QueryParam "from" Float :> QueryParam "to" Float :> QueryParam "user_id" UserId :> Get '[JSON] [ChartData]
                  :<|> ProtectedAPI)

type UsersPath = "users" :> QueryParam "q" T.Text :> QueryParam "page" PageNumber :> Get '[HTML] Html
type RootPath = Get '[HTML] Html
type CubemaniaAPI = PuzzleAPI
               :<|> "api" :> "users" :> QueryParam "q" T.Text :> Get '[JSON] [SimpleUser]
               :<|> UsersPath
               :<|> RootPath

type instance AuthServerData (AuthProtect "cookie-auth") = UserId

api :: Proxy CubemaniaAPI
api = Proxy

linkTo = safeLink api

usersLink :: Maybe PageNumber -> T.Text
usersLink page = "/" `T.append` (T.pack . show $ linkTo (Proxy :: Proxy UsersPath) Nothing page)

postLinkToComments :: AnnouncementId -> T.Text
postLinkToComments announcementId = postLink announcementId <> "#comments"

postLink :: AnnouncementId -> T.Text
postLink announcementId = "/posts/" <> T.pack (show announcementId)
