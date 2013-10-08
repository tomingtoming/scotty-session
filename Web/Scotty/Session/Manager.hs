{-# LANGUAGE OverloadedStrings #-}

module Web.Scotty.Session.Manager (
  SessionManager,
  SessionManagerInfo(SessionManagerInfo),
  def,
  delSession, delSessionVar, getSessionVar, setSessionVar
) where

import Data.ByteString
import Data.Default
import Web.Scotty

class SessionManager m where
  delSession    :: m -> ActionM ()
  delSessionVar :: m -> ByteString -> ActionM ()
  getSessionVar :: m -> ByteString -> ActionM (Maybe ByteString)
  setSessionVar :: m -> ByteString -> ByteString -> ActionM ()

data SessionManagerInfo = SessionManagerInfo {
  setSessionTimeoutMinutes :: Int
}

instance Default SessionManagerInfo where
  def = SessionManagerInfo { setSessionTimeoutMinutes = 30 }
