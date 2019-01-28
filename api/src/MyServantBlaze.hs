-- Backport of servant-blaze in order to pretty print HTML

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module MyServantBlaze where

import           Data.Typeable                 (Typeable)
import qualified Network.HTTP.Media            as M
import           Servant.API                   (Accept (..), MimeRender (..))
import           Text.Blaze.Html               (Html, ToMarkup, toHtml)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)

data HTML deriving Typeable

-- | @text/html;charset=utf-8@
instance Accept HTML where
    contentType _ = "text" M.// "html" M./: ("charset", "utf-8")

instance {-# OVERLAPPABLE #-} ToMarkup a => MimeRender HTML a where
    mimeRender _ = renderHtml . toHtml

instance {-# OVERLAPPING #-} MimeRender HTML Html where
    mimeRender _ = renderHtml
