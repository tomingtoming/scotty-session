{-# LANGUAGE OverloadedStrings #-}

module Web.Scotty.Session where

import Web.Scotty
import Web.Cookie
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Lbs
import qualified Data.Text as T
import qualified Data.Text.Lazy as Lt
import qualified Data.Text.Lazy.Encoding as Lt
import Blaze.ByteString.Builder (toLazyByteString)
import System.Random
import Data.Array (listArray, (!))

type SessionId = Lt.Text

sessionCookieKey :: Lt.Text
sessionCookieKey = "SCOTTYSESSION"

getSessionId :: ActionM (Maybe SessionId)
getSessionId = do
  cookie <- reqHeader "Cookie" :: ActionM (Maybe Lt.Text)
  return $ do
    cookiePairs <- fmap (parseCookiesText . lazyText2ByteString) cookie
    fmap Lt.fromStrict $ lookup (Lt.toStrict sessionCookieKey) cookiePairs

setSessionId :: SessionId -> ActionM ()
setSessionId sid = setHeader "Set-Cookie" cookie
  where
    cookie :: Lt.Text
    cookie = Lt.decodeUtf8 $ toLazyByteString $ renderSetCookie $ def {
      setCookieName  = lazyText2ByteString sessionCookieKey,
      setCookieValue = lazyText2ByteString sid
    }

genPlaneChar :: IO Char
genPlaneChar =
  let
    lst = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    arr = listArray (0, length lst - 1) lst
  in
    randomRIO (0, length lst - 1) >>= return . (arr !)

genSessionId :: IO SessionId
genSessionId = fmap Lt.pack $ (sequence . take 80 . repeat) genPlaneChar

lazyText2ByteString :: Lt.Text -> Bs.ByteString
lazyText2ByteString =  Lbs.toStrict . Lt.encodeUtf8

byteString2lazyText :: Bs.ByteString -> Lt.Text
byteString2lazyText =  Lt.decodeUtf8 . Lbs.fromStrict

