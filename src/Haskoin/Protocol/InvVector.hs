module Haskoin.Protocol.InvVector 
( InvVector(..) 
, InvType(..)
) where

import Control.Applicative ((<$>),(<*>))

import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)

import Haskoin.Crypto (Hash256)

data InvType = InvError | InvTx | InvBlock
    deriving (Eq, Show)

instance Binary InvType where

    get = go =<< getWord32le
        where go x = case x of
                0 -> return InvError
                1 -> return InvTx
                2 -> return InvBlock
                _ -> fail "bitcoinGet InvType: Invalid Type"

    put x = putWord32le $ case x of
                InvError -> 0
                InvTx    -> 1
                InvBlock -> 2

data InvVector = InvVector {
    invType :: !InvType,
    invHash :: !Hash256
} deriving (Eq, Show)

instance Binary InvVector where
    get = InvVector <$> get <*> get
    put (InvVector t h) = put t >> put h


