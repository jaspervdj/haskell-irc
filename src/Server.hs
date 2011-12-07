{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>))
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (fromException)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Data.Char (toUpper)
import Data.Maybe (catMaybes)

import Data.Aeson (ToJSON (..))
import Data.Aeson.Parser (json)
import Data.Attoparsec (parseOnly)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BC
import qualified Network.WebSockets as WS

import Event
import EventStore
import Irc.Message
import Irc.Message.Encode
import Irc.Socket
import Session
import User

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
        Message _ "353" (_ : _ : c : args)  ->
            sendClient $ Names c $ catMaybes $ map parseName $
                BC.words $ last args
        Message _ "376" _ ->
            sendClient Ready
        msg -> do
            sendClient $ Log $ encode msg
            putStrLn $ "Unhandled server event: " ++ show event
  where
    sendClient   = sendClientEvent mvar
    sendServer c = sendServerMessage mvar . makeMessage c

-- | Disconnect a client
disconnectClient :: MVar Session -> IO ()
disconnectClient mvar = modifyMVar_ mvar $ \session ->
    return $ session {sessionClient = ClientDisconnected}

-- Handle events sent by the client
handleClient :: WS.TextProtocol p
             => SessionStore -> MVar Session -> ThreadId -> WS.WebSockets p ()
handleClient sessionStore mvar serverThreadId =
    WS.catchWsError handle disconnect
  where
    sendClient   = liftIO . sendClientEvent mvar
    sendServer c = liftIO . sendServerMessage mvar . makeMessage c

    handle = do
        evt <- receiveClientEvent
        case evt of
            Disconnect    -> liftIO $ do
                sendServer "QUIT" ["haskell-irc-0.0.0.1"]
                user <- sessionUser <$> readMVar mvar
                deleteSession user sessionStore
                withEventStore $ deleteEvents user
                killThread serverThreadId
                putStrLn "Client disconnect, server thread killed"
            Join c _      -> do
                sendServer "JOIN" [c]
                sendClient evt
                handle
            Privmsg c _ t -> do
                sendServer "PRIVMSG" [c, t]
                sendClient evt
                handle
            _             -> do
                liftIO $ putStrLn $ "Unhandled client event: " ++ show evt
                handle

    disconnect exc = case fromException exc of
        Just WS.ConnectionClosed -> liftIO $ do
            disconnectClient mvar
            putStrLn "Client cleanly disconnected."
        _                        -> return ()

sendClientEvent :: MVar Session -> Event -> IO ()
sendClientEvent mvar event = do
    session <- readMVar mvar
    (obj, time) <- addTime (toJSON event)
    let bs = A.encode obj
    withEventStore $ putEvent (sessionUser session) time bs
    case sessionClient session of
        ClientDisconnected     -> return ()
        ClientConnected sender -> sender bs

sendServerMessage :: MVar Session -> Message -> IO ()
sendServerMessage mvar message = readMVar mvar >>= \session ->
    writeMessage (sessionServer session) message

handleConnect :: WS.TextProtocol p
              => SessionStore -> Event -> WS.WebSockets p ()
handleConnect sessionStore (Connect user) = do
    sink <- WS.getSink
    irc  <- liftIO $ connect (userServer user) (userPort user)

    let sender  = WS.sendSink sink . WS.textData
        client  = ClientConnected sender

    msession <- liftIO $ getSession user sessionStore
    mvar <- liftIO $ case msession of
        Just mvar -> do
            -- Update old session
            modifyMVar_ mvar $ \session -> do
                oldEvents <- withEventStore $ getEvents user
                mapM_ sender oldEvents
                return session {sessionClient = client}
            return mvar
        Nothing   -> do
            -- Start new session
            let session = Session user irc client
                nick    = userNick user
            mvar <- newMVar session
            putSession user mvar sessionStore

            -- Identify
            _ <- forkIO $ do
                threadDelay $ 1000 * 1000
                sendServerMessage mvar $ makeMessage "NICK" [userNick user]
                sendServerMessage mvar $ makeMessage "USER"
                    [BC.map toUpper nick, "*", "*", nick]
            return mvar

    -- Continue!
    serverThreadId <- liftIO $ forkIO $ handleServer mvar
    handleClient sessionStore mvar serverThreadId
handleConnect _ _ = error "Connect first!"

receiveClientEvent :: WS.TextProtocol p => WS.WebSockets p Event
receiveClientEvent = do
    bs <- WS.receiveData
    case parseOnly json bs of
        Left _    -> error $ "Could not parse: " ++ show bs
        Right val -> case A.fromJSON val of
            A.Error e   -> error $ "Invalid event: " ++ e
            A.Success x -> return x

app :: SessionStore -> WS.Request -> WS.WebSockets WS.Hybi00 ()
app sessionStore rq = do
    liftIO $ putStrLn "Client connected"
    WS.acceptRequest rq
    receiveClientEvent >>= handleConnect sessionStore

main :: IO ()
main = do
    sessionStore <- newSessionStore
    WS.runServer "0.0.0.0" 8001 $ app sessionStore
