{-# LANGUAGE OverloadedStrings #-}
import Data.Char (toUpper)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)

import Data.Aeson (ToJSON (..))
import Data.Aeson.Parser (json)
import Data.Attoparsec (parseOnly)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BC
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
    evt <- receiveClientEvent
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

handleConnect :: WS.TextProtocol p
              => SessionStore -> Event -> WS.WebSockets p ()
handleConnect sessions (Connect user) = do
    sink <- WS.getSink
    irc  <- liftIO $ connect (userServer user) (userPort user)

    let sender  = WS.sendSink sink . WS.textData
        client  = ClientConnected sender

    msession <- liftIO $ getSession user sessions
    mvar <- liftIO $ case msession of
        Just mvar -> do
            -- Update old session
            modifyMVar_ mvar $ \session -> do
                case sessionClient session of
                    ClientDisconnected msgs -> mapM_ sender msgs
                    _                       -> return ()
                return session {sessionClient = client}
            return mvar
        Nothing   -> do
            -- Start new session
            let session = Session user irc client
                nick    = userNick user
            mvar <- newMVar session
            putSession user mvar sessions

            -- Identify
            _ <- forkIO $ do
                threadDelay $ 1000 * 1000
                sendServerMessage mvar $ makeMessage "NICK" [userNick user]
                sendServerMessage mvar $ makeMessage "USER"
                    [BC.map toUpper nick, "*", "*", nick]
            return mvar

    -- Continue!
    _ <- liftIO $ forkIO $ handleServer mvar
    handleClient mvar
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
app sessions rq = do
    liftIO $ putStrLn "Client connected"
    WS.acceptRequest rq
    receiveClientEvent >>= handleConnect sessions

main :: IO ()
main = do
    store <- newSessionStore
    WS.runServer "0.0.0.0" 8282 $ app store
