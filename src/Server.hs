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
import qualified Data.ByteString as B
import qualified Network.WebSockets as WS

data ClientEvent
    = Connect ByteString Int ByteString
    deriving (Show)

instance FromJSON ClientEvent where
    parseJSON (A.Object o) = o .: "type" >>= \typ -> case (typ :: Text) of
        "connect" -> Connect <$> o .: "server" <*> o .: "port" <*> o .: "nick"
        _         -> mzero
    parseJSON _          = mzero

app :: WS.Request -> WS.WebSockets WS.Hybi00 ()
app rq = do
    liftIO $ putStrLn "Client connected"
    WS.acceptRequest rq
    forever $ do
        bs <- WS.receiveData
        case parseOnly json bs of
            Left _    -> liftIO $ putStrLn $ "Could not parse: " ++ show bs
            Right val -> liftIO (print val) >> case A.fromJSON val of
                A.Error e   -> liftIO $ do
                    putStrLn $ "Invalid event: " ++ show bs
                    putStrLn $ "Error: " ++ show e
                A.Success x -> liftIO $ putStrLn $
                    "Got: " ++ show (x :: ClientEvent)
        WS.sendTextData bs

main :: IO ()
main = WS.runServer "0.0.0.0" 8282 app
