module Haskoin.Protocol.Addr 
( Addr(..)
, NetworkAddressTime 
) where

import Control.Monad (liftM2, replicateM, forM_)
import Control.Applicative ((<$>))

import Data.Word (Word32)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)

import Haskoin.Protocol.VarInt
import Haskoin.Protocol.NetworkAddress

type NetworkAddressTime = (Word32, NetworkAddress)

data Addr = Addr {
    addrList :: ![NetworkAddressTime]
} deriving (Eq, Show)

instance Binary Addr where

    get = Addr <$> (repList =<< get)
        where repList (VarInt c) = replicateM (fromIntegral c) action
              action             = liftM2 (,) getWord32le get 

    put (Addr xs) = do
        put $ VarInt $ fromIntegral $ length xs
        forM_ xs $ \(a,b) -> (putWord32le a) >> (put b)

