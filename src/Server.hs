{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (forever, mzero)
import Control.Monad.Trans (liftIO)
import Control.Applicative ((<$>), (<*>))

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

data ClientEvent
    = Connect ByteString Int ByteString
    | Join ByteString
    deriving (Show)

instance FromJSON ClientEvent where
    parseJSON (A.Object o) = o .: "type" >>= \typ -> case (typ :: Text) of
        "connect" -> Connect <$> o .: "server" <*> o .: "port" <*> o .: "nick"
        "join"    -> Join    <$> o .: "channel"
        _         -> mzero
    parseJSON _          = mzero

handleConnect :: WS.TextProtocol p => ClientEvent -> WS.WebSockets p ()
handleConnect (Connect server port nick) = do
    sink <- WS.getSink
    Right mirc <- liftIO $ IRC.connect (config sink) True True 
    forever $ do
        evt <- receiveClientEvent
        case evt of
            Join channel -> liftIO $
                IRC.sendCmd mirc $ IRC.MJoin channel Nothing
            _            -> error $ "Did not expect" ++ show evt
        WS.sendTextData $ T.pack $ show evt
  where
    config sink = IRC.defaultConfig
        { IRC.cAddr   = BC.unpack server
        , IRC.cPort   = port
        , IRC.cNick   = BC.unpack nick
        , IRC.cEvents = [event sink]
        }

    event sink = IRC.RawMsg $ \_ msg ->
        WS.sendSink sink $ WS.textData $ IRC.mRaw msg
handleConnect _ = error "Connect first!"

receiveClientEvent :: WS.TextProtocol p => WS.WebSockets p ClientEvent
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
