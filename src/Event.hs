{-# LANGUAGE OverloadedStrings #-}
module Event
    ( Event (..)
    , parseName
    , addTime
    ) where

import Control.Monad (mzero)
import Control.Applicative (pure, (<$>), (<*>))
import System.Locale (defaultTimeLocale)

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (formatTime, getCurrentTime)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M

import User

data Event
    = Log        ByteString
    | Connect    User
    | Join       ByteString ByteString
    | Privmsg    ByteString ByteString ByteString
    | Topic      ByteString ByteString
    | Names      ByteString [Name]
    | Ready
    | Disconnect
    deriving (Show)

instance FromJSON Event where
    parseJSON (A.Object o) = o .: "type" >>= \typ -> case (typ :: Text) of
        "log"        -> Log        <$> o .: "text"
        "connect"    -> Connect    <$> o .: "user"
        "join"       -> Join    <$> o .: "channel" <*> o .: "nick"
        "privmsg"    ->
            Privmsg <$> o .: "channel" <*> o .: "nick" <*> o .: "text"
        "topic"      -> Topic      <$> o .: "channel" <*> o .: "text"
        "names"      -> Names      <$> o .: "channel" <*> o .: "names"
        "ready"      -> pure Ready
        "disconnect" -> pure Disconnect
        _            -> mzero
    parseJSON _          = mzero

instance ToJSON Event where
    toJSON e = obj $ case e of
        Log        t     -> ["text" .= t]
        Connect    u     -> ["user" .= u]
        Join       c n   -> ["channel" .= c, "nick" .= n]
        Privmsg    c n t -> ["channel" .= c, "nick" .= n, "text" .= t]
        Topic      c t   -> ["channel" .= c, "text" .= t]
        Names      c n   -> ["channel" .= c, "names" .= n]
        Ready            -> []
        Disconnect       -> []
      where
        obj = A.object . ("type" .= eventType e :)

eventType :: Event -> ByteString
eventType (Log _)         = "log"
eventType (Connect _)     = "connect"
eventType (Join _ _)      = "join"
eventType (Privmsg _ _ _) = "privmsg"
eventType (Topic _ _)     = "topic"
eventType (Names _ _)     = "names"
eventType Ready           = "ready"
eventType Disconnect      = "disconnect"

data Name = Name ByteString ByteString
    deriving (Show)

instance FromJSON Name where
    parseJSON (A.Object o) = Name <$> o .: "prefix" <*> o .: "nick"
    parseJSON _            = mzero

instance ToJSON Name where
    toJSON (Name p n) = A.object ["prefix" .= p, "nick" .= n]

parseName :: ByteString -> Maybe Name
parseName bs = case BC.uncons bs of
    Nothing       -> Nothing
    Just (_,  "") -> Nothing
    Just ('#', _) -> Nothing
    Just ('@', n) -> Just $ Name "@" n
    _             -> Just $ Name "" bs

addTime :: A.Value -> IO (A.Value, Double)
addTime (A.Object o) = do
    str <- formatTime defaultTimeLocale "%s" <$> getCurrentTime
    let time = read str :: Integer
        obj  = A.Object $ M.insert "time" (A.Number $ fromIntegral time) o
    return (obj, fromIntegral time)
addTime x            = return (x, 0)
