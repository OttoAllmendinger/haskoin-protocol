module Haskoin.Protocol.Ping
( Ping(..)
, Pong(..)
) where

import Control.Applicative ((<$>))

import Data.Word (Word64)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord64le)
import Data.Binary.Put (putWord64le)

newtype Ping = Ping { pingNonce :: Word64 } 
    deriving (Eq, Show)

newtype Pong = Pong { pongNonce :: Word64 } 
    deriving (Eq, Show)

instance Binary Ping where
    get = Ping <$> getWord64le
    put (Ping n) = putWord64le n

instance Binary Pong where
    get = Pong <$> getWord64le
    put (Pong n) = putWord64le n

