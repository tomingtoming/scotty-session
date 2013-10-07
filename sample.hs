{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Network (withSocketsDo)
import Web.Scotty
import Web.Scotty.Session

main :: IO ()
main = do
  withSocketsDo $ scotty 1986 $ do
    get "/" $ do
      getSid <- getSessionId
      newSid <- liftIO genSessionId
      setSessionId newSid
      case getSid of
        Just oldSid -> html $ mconcat ["<p>Old Key: ", oldSid, "</p><p>New key: ", newSid, "</p>"]
        Nothing     -> html $ mconcat ["<p>Session Not Found!</p><p>New key: ", newSid, "</p>"]
