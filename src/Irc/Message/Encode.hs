{-# LANGUAGE OverloadedStrings #-}
module Irc.Message.Encode
    ( encodePrefix
    , encode
    ) where

import Data.Maybe (isJust, fromMaybe)
import Data.Monoid (Monoid, mappend, mempty, mconcat)

import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Char8 as SBC

import Irc.Message

encodePrefix :: Prefix -> ByteString
encodePrefix (ServerPrefix s)   = ":" `mappend` s
encodePrefix (NickPrefix n u h) = mconcat
    [ ":", n, fromMaybe "" (fmap ("!" `mappend`) u)
    , fromMaybe "" (fmap ("@" `mappend`) h)
    ]

encodeCommand :: ByteString -> ByteString
encodeCommand = id

encodeParameters :: [ByteString] -> ByteString
encodeParameters []                                 = mempty
encodeParameters (x : [])
    | hasSpace x || SBC.null x || SBC.head x == ':' = " :" `mappend` x
    | otherwise                                     = " " `mappend` x
  where
    hasSpace = isJust . SBC.find (== ' ')
encodeParameters (x : xs)                           =
    " " `mappend` x `mappend` encodeParameters xs

encode :: Message -> ByteString
encode (Message p c ps) =
    encodePrefix' p `mappend` encodeCommand c `mappend` encodeParameters ps
  where
    encodePrefix' = fromMaybe mempty . fmap ((`mappend` " ") . encodePrefix)
