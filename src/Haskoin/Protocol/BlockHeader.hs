module Haskoin.Protocol.BlockHeader ( BlockHeader(..) ) where

import Control.Applicative ((<$>),(<*>))

import Data.Word (Word32)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)

import Haskoin.Crypto (Hash256)

data BlockHeader = BlockHeader {
    blockVersion   :: !Word32,
    prevBlock      :: !Hash256,
    merkleRoot     :: !Hash256,
    blockTimestamp :: !Word32,
    blockBits      :: !Word32,
    bhNonce        :: !Word32
} deriving (Eq, Show)

instance Binary BlockHeader where

    get = BlockHeader <$> getWord32le
                      <*> get
                      <*> get
                      <*> getWord32le
                      <*> getWord32le
                      <*> getWord32le

    put (BlockHeader v p m bt bb n) = do
        putWord32le v
        put         p
        put         m
        putWord32le bt
        putWord32le bb
        putWord32le n 

