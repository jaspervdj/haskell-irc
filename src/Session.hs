{-# LANGUAGE OverloadedStrings #-}
module Session
    ( User (..)
    , ClientState (..)
    , Session (..)
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

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
    } deriving (Eq, Show)

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
