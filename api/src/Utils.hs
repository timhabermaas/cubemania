{-# LANGUAGE OverloadedStrings #-}

module Utils
    ( formatTime
    , humanizeTimeInterval
    , safeRead
    , safeHead
    , hashPassword
    , hashNewPassword
    , authenticate
    , newSalt
    , gravatarHash
    , sessionIdFromByteString
    ) where

import Data.Text (Text, pack)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import Text.Printf (printf)
import Data.Monoid ((<>))
import Crypto.Hash
import Crypto.Random.Entropy
import Types
import Control.Monad.IO.Class
import qualified Data.ByteString.Base64 as Base64

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_xs) = Just x

safeRead :: Read a => String -> Maybe a
safeRead x =
  case reads x of
    [(i, _rest)] -> Just i
    _ -> Nothing

formatTime :: DurationInMs -> Text
formatTime time
    | (round $ (((fromIntegral time) / 1000 :: Double)) :: Int) < 60 = let s = ((fromIntegral time) / 1000)
                     in  pack $ printf "%.2fs" (s :: Float)
    | otherwise    = let hs = round $ ((fromIntegral time :: Double) / 10) :: Int
                         minutes = floor $ (fromIntegral hs :: Double) / 6000 :: Int
                         s' = fromIntegral (hs - (fromIntegral $ minutes * 6000)) / 100
                     in  pack $ printf "%d:%05.2fmin" minutes (s' :: Double)

msToDays :: (Integral a2) => a2 -> Double
msToDays t = (fromIntegral t) / (24 * 60 * 60 * 1000)
msToHours :: (Integral a2) => a2 -> Double
msToHours t = (fromIntegral t) / (60 * 60 * 1000)

humanizeTimeInterval :: Integer -> Text
humanizeTimeInterval t
    | msToDays t >= 0.5 = pluralize (round $ msToDays t :: Int) "day"
    | msToHours t >= 0.5 = pluralize (round $ msToHours t :: Int) "hour"
    | otherwise = "less than an hour"

pluralize :: (Integral a, Show a) => a -> Text -> Text
pluralize x t = (pack $ show x) <> " " <> t <> suffix
  where
    suffix = if x > 1 then "s" else ""

gravatarHash :: Text -> Text
gravatarHash email = pack $ show $ (hash (TE.encodeUtf8 email) :: Digest MD5)

hashPassword :: Salt -> ClearPassword -> HashedPassword
hashPassword (Salt salt) (ClearPassword password) = HashedPassword $ BS.pack $ show $ (hash ((TE.encodeUtf8 password) <> salt) :: Digest SHA256)

authenticate :: (Salt, ClearPassword) -> HashedPassword -> Bool
authenticate (s, cp) hp = hashPassword s cp == hp

newSalt :: MonadIO m => m Salt
newSalt = liftIO $ Salt . Base64.encode <$> getEntropy 6

hashNewPassword :: MonadIO m => ClearPassword -> m (HashedPassword, Salt)
hashNewPassword password = do
    salt <- newSalt
    let hashedPassword = hashPassword salt password
    return (hashedPassword, salt)

sessionIdFromByteString :: BS.ByteString -> Maybe SessionId
sessionIdFromByteString = fmap SessionId . safeRead . BS.unpack
