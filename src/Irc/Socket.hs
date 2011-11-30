-- | Low-level socket code for the IRC bot
--
{-# LANGUAGE OverloadedStrings #-}
module Irc.Socket
    ( IrcSocket
    , connect
    , readMessage
    , writeMessage
    ) where

import Control.Applicative ((<$>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Monoid (mappend)

import Data.ByteString (ByteString)
import Network.Socket.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Network.Socket as S

import Irc.Message
import Irc.Message.Decode
import Irc.Message.Encode

data IrcSocket = IrcSocket
    { ircSocket    :: S.Socket
    , ircRemainder :: IORef ByteString
    }

connect :: ByteString
        -> Int
        -> IO IrcSocket
connect host port = do
    let host' = BC.unpack host
    addrInfo <- head <$> S.getAddrInfo Nothing (Just host') (Just $ show port)
    sock <- S.socket (S.addrFamily addrInfo) S.Stream S.defaultProtocol
    S.connect sock $ S.addrAddress addrInfo
    ref <- newIORef ""
    return $ IrcSocket sock ref

readMessage :: IrcSocket -> IO Message
readMessage socket = do
    previous <- readIORef $ ircRemainder socket
    consume previous
  where
    loop previous = do
        chunk <- recv (ircSocket socket) 4096
        if (B.null chunk)
            then error "Connection closed"
            else consume $ mappend previous chunk

    consume chunk =
        let (line, rest) = BC.breakSubstring "\r\n" chunk
        in if B.null rest
                then -- We don't have a line yet
                     loop chunk
                else -- We have at least one line to consume
                     do writeIORef (ircRemainder socket) (B.drop 2 rest)
                        B.putStrLn $ ">> " `mappend` line
                        let Just msg = decode line
                        return msg

writeMessage :: IrcSocket -> Message -> IO ()
writeMessage socket msg = do
    let line = encode msg
    B.putStrLn $ "<< " `mappend` line
    sendAll (ircSocket socket) $ line `mappend` "\r\n"
