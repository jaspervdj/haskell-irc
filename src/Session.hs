{-# LANGUAGE OverloadedStrings #-}
module Session
    ( ClientState (..)
    , Session (..)
    , SessionStore
    , newSessionStore
    , getSession
    , putSession
    , deleteSession
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.ByteString.Lazy as BL

import Irc.Socket
import User

data ClientState
    = ClientConnected (BL.ByteString -> IO ())
    | ClientDisconnected

data Session = Session
    { sessionUser   :: User
    , sessionServer :: IrcSocket
    , sessionClient :: ClientState
    }

newtype SessionStore = SessionStore (MVar (Map User (MVar Session)))

newSessionStore :: IO SessionStore
newSessionStore = SessionStore <$> newMVar M.empty

getSession :: User -> SessionStore -> IO (Maybe (MVar Session))
getSession u (SessionStore m) = M.lookup u <$> readMVar m

putSession :: User -> MVar Session -> SessionStore -> IO ()
putSession u s (SessionStore m) = modifyMVar_ m $ return . M.insert u s

deleteSession :: User -> SessionStore -> IO ()
deleteSession u (SessionStore m) = modifyMVar_ m $ return . M.delete u
