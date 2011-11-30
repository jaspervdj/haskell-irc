{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (forever, mzero)
import Control.Monad.Trans (liftIO)
import Control.Applicative (pure, (<$>), (<*>))

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as A
import Data.Aeson.Parser (json)
import Data.Attoparsec (parseOnly)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Network.WebSockets as WS
import qualified Network.SimpleIRC as IRC

data Event
    = Connect ByteString Int ByteString
    | Join    ByteString ByteString
    | Privmsg ByteString ByteString ByteString
    | Topic   ByteString ByteString
    deriving (Show)

instance FromJSON Event where
    parseJSON (A.Object o) = o .: "type" >>= \typ -> case (typ :: Text) of
        "connect" -> Connect <$> o .: "server"  <*> o .: "port" <*> o .: "nick"
        "join"    -> Join    <$> o .: "channel" <*> o .: "nick"
        "privmsg" -> Privmsg <$> o .: "channel" <*> o .: "nick" <*> o .: "text"
        "topic"   -> Topic   <$> o .: "channel" <*> o .: "text"
        _         -> mzero
    parseJSON _          = mzero

instance ToJSON Event where
    toJSON e = obj $ case e of
        Connect s p n -> ["server" .= s,  "port" .= p, "nick" .= n]
        Join    c n   -> ["channel" .= c, "nick" .= n]
        Privmsg c n t -> ["channel" .= c, "nick" .= n, "text" .= t]
        Topic   c t   -> ["channel" .= c, "text" .= t]
      where
        obj = A.object . ("type" .= eventType e :)

eventType :: Event -> ByteString
eventType (Connect _ _ _) = "connect"
eventType (Join _ _)      = "join"
eventType (Privmsg _ _ _) = "privmsg"
eventType (Topic _ _)     = "topic"

handleConnect :: WS.TextProtocol p => Event -> WS.WebSockets p ()
handleConnect (Connect server port nick) = do
    sink <- WS.getSink
    let sendEvent = WS.sendSink sink . WS.textData . A.encode
    Right mirc <- liftIO $ IRC.connect (config sink) True True 
    forever $ do
        evt <- receiveClientEvent
        case evt of
            Join c _      -> liftIO $ do
                IRC.sendCmd mirc $ IRC.MJoin c Nothing
                sendEvent evt
            Privmsg c _ t -> liftIO $ do
                IRC.sendCmd mirc $ IRC.MPrivmsg c t
                sendEvent evt
            _             ->
                error $ "Did not expect" ++ show evt
        WS.sendTextData $ T.pack $ show evt
  where
    config sink = IRC.defaultConfig
        { IRC.cAddr   = BC.unpack server
        , IRC.cPort   = port
        , IRC.cNick   = BC.unpack nick
        , IRC.cEvents = handleServerEvent sink
        }

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
