{-# LANGUAGE OverloadedStrings #-}
import Data.Char (toUpper)
import Control.Monad (forever, mzero)
import Control.Monad.Trans (liftIO)
import Control.Applicative (pure, (<$>), (<*>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import System.Locale (defaultTimeLocale)

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import Data.Aeson.Parser (json)
import Data.Attoparsec (parseOnly)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (formatTime, getCurrentTime)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import Event
import Irc.Message
import Irc.Message.Encode
import Irc.Socket
import Session

-- | Handle events sent by the server
handleServer :: MVar Session -> IO ()
handleServer mvar = forever $ do
    server <- sessionServer <$> readMVar mvar
    event  <- readMessage server
    case event of
        Message _ "PRIVMSG" [c, t] ->
            sendClient $ Privmsg c (getNick event) t
        Message _ "PING" x ->
            sendServer "PONG" x
        Message _ "332" [_, c, t] ->
            sendClient $ Topic c t
        Message _ "353" (_ : "=" : c : args)  ->
            sendClient $ Names c $ map parseName $ BC.words $ last args
        Message _ "376" _ ->
            sendClient Ready
        msg -> do
            sendClient $ Log $ encode msg
            putStrLn $ "Unhandled server event: " ++ show event
  where
    sendClient   = sendClientEvent mvar
    sendServer c = sendServerMessage mvar . makeMessage c

    -- Handle events sent by the client
handleClient :: WS.TextProtocol p => MVar Session -> WS.WebSockets p ()
handleClient mvar = forever $ do
    liftIO $ print "Reading from client..."
    evt <- receiveClientEvent
    liftIO $ print evt
    case evt of
        Join c _      -> liftIO $ do
            sendServer "JOIN" [c]
            sendClient evt
        Privmsg c _ t -> liftIO $ do
            sendServer "PRIVMSG" [c, t]
            sendClient evt
        _             -> liftIO $
            putStrLn $ "Unhandled client event: " ++ show evt
  where
    sendClient   = sendClientEvent mvar
    sendServer c = sendServerMessage mvar . makeMessage c

sendClientEvent :: MVar Session -> Event -> IO ()
sendClientEvent mvar event = modifyMVar_ mvar $ \session -> do
    bs <- A.encode <$> addTime (toJSON event)
    case sessionClient session of
        ClientDisconnected bss -> return $
            session {sessionClient = ClientDisconnected (bs : bss)}
        ClientConnected sender -> do
            sender bs
            return session

sendServerMessage :: MVar Session -> Message -> IO ()
sendServerMessage mvar message = readMVar mvar >>= \session ->
    writeMessage (sessionServer session) message

handleConnect :: WS.TextProtocol p => Event -> WS.WebSockets p ()
handleConnect (Connect user) = do
    sink <- WS.getSink
    irc  <- liftIO $ connect (userServer user) (userPort user)

    let sender  = WS.sendSink sink . WS.textData
        client  = ClientConnected sender
        session = Session user irc client

    mvar <- liftIO $ newMVar session

    let nick = userNick user
        sendServer c p = writeMessage irc $ makeMessage c p

    -- Identify
    _ <- liftIO $ forkIO $ do
        threadDelay $ 1000 * 1000
        sendServerMessage mvar $ makeMessage "NICK" [userNick user]
        sendServerMessage mvar $ makeMessage "USER"
            [BC.map toUpper nick, "*", "*", nick]

    _ <- liftIO $ forkIO $ handleServer mvar
    handleClient mvar
handleConnect _ = error "Connect first!"

receiveClientEvent :: WS.TextProtocol p => WS.WebSockets p Event
receiveClientEvent = do
    bs <- WS.receiveData
    case parseOnly json bs of
        Left _    -> error $ "Could not parse: " ++ show bs
        Right val -> case A.fromJSON val of
            A.Error e   -> error $ "Invalid event: " ++ e
            A.Success x -> return x

app :: WS.Request -> WS.WebSockets WS.Hybi00 ()
app rq = do
    liftIO $ putStrLn "Client connected"
    WS.acceptRequest rq
    receiveClientEvent >>= handleConnect

main :: IO ()
main = WS.runServer "0.0.0.0" 8282 app
