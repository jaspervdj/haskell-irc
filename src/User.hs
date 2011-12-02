{-# LANGUAGE OverloadedStrings #-}
module User
    ( User (..)
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import Data.ByteString (ByteString)
import qualified Data.Aeson as A

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
