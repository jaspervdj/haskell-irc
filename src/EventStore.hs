{-# LANGUAGE OverloadedStrings #-}
module EventStore
    ( EventStore
    , newEventStore
    , putEvent
    , getEvents
    ) where

import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import Data.Monoid (mconcat)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Database.Redis.Redis as R

import User

newtype EventStore = EventStore R.Redis

newEventStore :: IO EventStore
newEventStore = EventStore <$> R.connect "127.0.0.1" R.defaultPort

makeKey :: User -> ByteString
makeKey (User s p n pw) = mconcat [s, "/", BC.pack (show p), "/", n, "/", pw]

putEvent :: User -> Double -> BL.ByteString -> EventStore -> IO ()
putEvent user s event (EventStore r) = do
    _ <- R.zadd r (makeKey user) s event
    return ()

getEvents :: User -> EventStore -> IO [BL.ByteString]
getEvents user (EventStore r) = do
    R.RMulti (Just replies) <- R.zrange r key R.takeAll False
    catMaybes <$> mapM R.fromRBulk replies
  where
    key = makeKey user
