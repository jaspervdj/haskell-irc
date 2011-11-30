{-# LANGUAGE OverloadedStrings #-}
module Irc.Message
    ( Prefix (..)
    , Message (..)
    , getNick
    , makeMessage
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()

data Prefix
    = ServerPrefix ByteString
    | NickPrefix ByteString (Maybe ByteString) (Maybe ByteString)
    deriving (Show, Eq)

data Message = Message
    { messagePrefix     :: Maybe Prefix
    , messageCommand    :: ByteString
    , messageParameters :: [ByteString]
    } deriving (Show)

getNick :: Message -> ByteString
getNick (Message (Just (ServerPrefix n))   _ _) = n
getNick (Message (Just (NickPrefix n _ _)) _ _) = n
getNick _                                       = "???"

makeMessage :: ByteString -> [ByteString] -> Message
makeMessage = Message Nothing
