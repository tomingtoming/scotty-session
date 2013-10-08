{-# LANGUAGE OverloadedStrings #-}

module Web.Scotty.Session.Id (
  SessionId,
  getSessionId,
  setSessionId,
  genSessionId
) where

import Web.Scotty
import Web.Cookie
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as BsC8
import qualified Data.ByteString.Lazy as Lbs
import qualified Data.Text.Lazy as Lt
import qualified Data.Text.Lazy.Encoding as Lt
import Blaze.ByteString.Builder (toLazyByteString)
import System.Random
import Data.Array (listArray, (!))

type SessionId = Bs.ByteString

sessionCookieKey :: Bs.ByteString
sessionCookieKey = "SCTYSID"

getSessionId :: ActionM (Maybe SessionId)
getSessionId = do
  cookie <- reqHeader "Cookie" :: ActionM (Maybe Lt.Text)
  return $ do
    cookiePairs <- fmap (parseCookies . lazyText2ByteString) cookie
    lookup sessionCookieKey cookiePairs

setSessionId :: SessionId -> ActionM ()
setSessionId sid = setHeader "Set-Cookie" cookie
  where
    cookie :: Lt.Text
    cookie = (Lt.decodeUtf8 . toLazyByteString . renderSetCookie) def {
      setCookieName  = sessionCookieKey,
      setCookieValue = sid
    }

genPlaneChar :: IO Char
genPlaneChar =
  let
    lst = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    arr = listArray (0, length lst - 1) lst
  in
    randomRIO (0, length lst - 1) >>= return . (arr !)

genSessionId :: IO SessionId
genSessionId = fmap BsC8.pack $ (sequence . take 64 . repeat) genPlaneChar

lazyText2ByteString :: Lt.Text -> Bs.ByteString
lazyText2ByteString =  Lbs.toStrict . Lt.encodeUtf8
