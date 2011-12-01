{-# LANGUAGE OverloadedStrings #-}
module Session
    ( User (..)
    , ClientState (..)
    , Session (..)
    , SessionStore
    , newSessionStore
    , getSession
    , putSession
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Monad (mzero)
import Data.Map (Map)
import qualified Data.Map as M

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A

import Irc.Socket

data User = User
    { userServer   :: ByteString
    , userPort     :: Int
    , userNick     :: ByteString
    , userPassword :: ByteString
    } deriving (Eq, Ord, Show)

instance FromJSON User where
    parseJSON (A.Object o) = User <$>
        o .: "server" <*> o .: "port" <*> o .: "nick" <*> o .: "password"
    parseJSON _            = mzero

instance ToJSON User where
    toJSON (User s p n pw) =
        A.object ["server" .= s, "port" .= p, "nick" .= n, "password" .= pw]

data ClientState
    = ClientConnected    (BL.ByteString -> IO ())
    | ClientDisconnected [BL.ByteString]

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
