{-# LANGUAGE OverloadedStrings #-}
import Data.Char (toUpper)
import Control.Monad (forever, mzero)
import Control.Monad.Trans (liftIO)
import Control.Applicative (pure, (<$>), (<*>))
import Control.Concurrent (forkIO, threadDelay)

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import Data.Aeson.Parser (json)
import Data.Attoparsec (parseOnly)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Network.SimpleIRC as IRC
import qualified Network.WebSockets as WS

import Irc.Message
import Irc.Socket

data Event
    = Connect ByteString Int ByteString
    | Join    ByteString ByteString
    | Privmsg ByteString ByteString ByteString
    | Topic   ByteString ByteString
    | Names   ByteString [Name]
    deriving (Show)

instance FromJSON Event where
    parseJSON (A.Object o) = o .: "type" >>= \typ -> case (typ :: Text) of
        "connect" -> Connect <$> o .: "server"  <*> o .: "port" <*> o .: "nick"
        "join"    -> Join    <$> o .: "channel" <*> o .: "nick"
        "privmsg" -> Privmsg <$> o .: "channel" <*> o .: "nick" <*> o .: "text"
        "topic"   -> Topic   <$> o .: "channel" <*> o .: "text"
        "names"   -> Names   <$> o .: "channel" <*> o .: "names"
        _         -> mzero
    parseJSON _          = mzero

instance ToJSON Event where
    toJSON e = obj $ case e of
        Connect s p n -> ["server" .= s,  "port" .= p, "nick" .= n]
        Join    c n   -> ["channel" .= c, "nick" .= n]
        Privmsg c n t -> ["channel" .= c, "nick" .= n, "text" .= t]
        Topic   c t   -> ["channel" .= c, "text" .= t]
        Names   c n   -> ["channel" .= c, "names" .= n]
      where
        obj = A.object . ("type" .= eventType e :)

eventType :: Event -> ByteString
eventType (Connect _ _ _) = "connect"
eventType (Join _ _)      = "join"
eventType (Privmsg _ _ _) = "privmsg"
eventType (Topic _ _)     = "topic"
eventType (Names _ _)     = "names"

data Name = Name ByteString ByteString
    deriving (Show)

instance FromJSON Name where
    parseJSON (A.Object o) = Name <$> o .: "prefix" <*> o .: "nick"
    parseJSON _            = mzero

instance ToJSON Name where
    toJSON (Name p n) = A.object ["prefix" .= p, "nick" .= n]

parseName :: ByteString -> Name
parseName bs = case BC.uncons bs of
    Just ('@', n) -> Name "@" n
    _             -> Name "" bs

handleConnect :: WS.TextProtocol p => Event -> WS.WebSockets p ()
handleConnect (Connect server port nick) = do
    sink <- WS.getSink
    let sendClient = WS.sendSink sink . WS.textData . A.encode

    irc <- liftIO $ connect server port
    let sendServer c p = writeMessage irc $ makeMessage c p

    -- Identify
    _ <- liftIO $ forkIO $ do
        threadDelay $ 1000 * 1000
        sendServer "NICK" [nick]
        sendServer "USER" [BC.map toUpper nick, "*", "*", nick]

    -- Handle events sent by the server
    _ <- liftIO $ forkIO $ forever $ do
        evt <- readMessage irc
        case evt of
            Message _ "PRIVMSG" [c, t] ->
                sendClient $ Privmsg c (getNick evt) t
            Message _ "PING" x ->
                sendServer "PONG" x
            Message _ "332" [_, c, t] ->
                sendClient $ Topic c t
            Message _ "353" (_ : "=" : c : args)  ->
                sendClient $ Names c $ map parseName $ BC.words $ last args
            _                          ->
                putStrLn $ "Unhandled server event: " ++ show evt
        return ()

    -- Handle events sent by the client
    forever $ do
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
        WS.sendTextData $ T.pack $ show evt
handleConnect _ = error "Connect first!"

handleServerEvent :: WS.TextProtocol p => WS.Sink p -> [IRC.IrcEvent]
handleServerEvent sink =
    [ IRC.Privmsg $ \_ msg -> maybe (return ()) sendEvent $
        Privmsg <$> IRC.mOrigin msg <*> IRC.mNick msg <*> pure (IRC.mMsg msg)
    , IRC.Numeric $ \_ msg -> (print msg) >> case IRC.mCode msg of
        -- Topic
        "332" -> maybe (return ()) sendEvent $
            Topic <$> IRC.mChan msg <*> pure (IRC.mMsg msg)
        _     -> return ()
    , IRC.Notice pipe
    , IRC.RawMsg pipe
    ]
  where
    pipe _ msg = WS.sendSink sink $ WS.textData $ IRC.mRaw msg
    sendEvent  = WS.sendSink sink . WS.textData . A.encode

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
