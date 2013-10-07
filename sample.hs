{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Web.Scotty.Session
import Network.Wai.Parse
import Network
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as Lbs
import Data.Monoid (mconcat)
import Data.Text.Lazy
import Control.Monad.IO.Class

main = withSocketsDo $ scotty 1986 $ do
  get "/" $ do
    msid <- getSessionId
    liftIO $ putStrLn $ show msid
    sid <- liftIO $ genSessionId
    setSessionId sid
    case msid of Just sid -> html "<p>OK!</p>"
                 Nothing  -> html "<p>Session Not Found!</p>"
