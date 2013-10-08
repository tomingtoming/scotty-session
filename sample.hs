{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Network (withSocketsDo)
import Web.Scotty
import Web.Scotty.Session
import Web.Scotty.Session.Id
import Web.Scotty.Session.Manager
import Web.Scotty.Session.Manager.Pure

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as BsC8
import qualified Data.ByteString.Lazy as Lbs
import qualified Data.Text.Lazy as Lt
import qualified Data.Text.Lazy.Encoding as Lt

main :: IO ()
main = do
  sm <- newPureSessionManager def
  withSocketsDo $ scotty 1986 $ do
    get "/session/:key" $ do
      key <- param "key"
      mv <- getSessionVar sm key
      case mv of
        Just v  -> text $ bs2lt v
        Nothing -> status 404

bs2lt = Lt.decodeUtf8 . Lbs.fromStrict
lt2bs = Lbs.toStrict . Lt.encodeUtf8
