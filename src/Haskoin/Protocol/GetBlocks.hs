module Haskoin.Protocol.GetBlocks 
( GetBlocks(..) 
, BlockLocator
) where

import Control.Monad (replicateM, forM_)
import Control.Applicative ((<$>),(<*>))

import Data.Word (Word32)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)

import Haskoin.Protocol.VarInt
import Haskoin.Crypto (Hash256)

type BlockLocator = [Hash256]

data GetBlocks = GetBlocks {
    getBlocksVersion  :: !Word32,
    getBlocksLocator  :: !BlockLocator,
    getBlocksHashStop :: !Hash256
} deriving (Eq, Show)

instance Binary GetBlocks where

    get = GetBlocks <$> getWord32le
                    <*> (repList =<< get)
                    <*> get
        where repList (VarInt c) = replicateM (fromIntegral c) get

    put (GetBlocks v xs h) = do
        putWord32le v
        put $ VarInt $ fromIntegral $ length xs
        forM_ xs put
        put h

