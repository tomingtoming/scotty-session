{-# LANGUAGE OverloadedStrings #-}

module Web.Scotty.Session.Manager.Pure(
  newPureSessionManager,
  delSession, delSessionVar, getSessionVar, setSessionVar
) where

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Data.ByteString as Bs
import Data.ByteString.Lazy as Lbs
import Data.Map as Map
import Data.Text.Lazy.Encoding as Lt
import Web.Scotty
import Web.Scotty.Session.Id
import Web.Scotty.Session.Manager

data PureSessionManager = PureSessionManager {
  setStrage :: TVar (Map Bs.ByteString (TVar (Map Bs.ByteString Bs.ByteString))),
  setInfo :: SessionManagerInfo
}

newPureSessionManager :: SessionManagerInfo -> IO PureSessionManager
newPureSessionManager i = do
  m <- newTVarIO Map.empty
  return $ PureSessionManager { setStrage = m, setInfo = i }

instance SessionManager PureSessionManager where
  -- m -> ActionM ()
  delSession (PureSessionManager m i) = do
    maybeSid <- getSessionId
    case maybeSid of
      Just sid -> liftIO $ atomically $ modifyTVar m (\m' -> Map.delete sid m')
      Nothing  -> return ()
  -- m -> ByteString -> ActionM ()
  delSessionVar (PureSessionManager m i) = undefined
  -- m -> ByteString -> ActionM (Maybe ByteString)
  getSessionVar (PureSessionManager m i) k = undefined
  -- m -> ByteString -> ByteString -> ActionM ()
  setSessionVar (PureSessionManager m i) k = undefined
