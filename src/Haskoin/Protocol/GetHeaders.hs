module Haskoin.Protocol.GetHeaders ( GetHeaders(..) ) where

import Control.Monad (replicateM, forM_)
import Control.Applicative ((<$>),(<*>))

import Data.Word (Word32)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)

import Haskoin.Protocol.VarInt
import Haskoin.Protocol.GetBlocks (BlockLocator)

import Haskoin.Crypto (Hash256)

data GetHeaders = GetHeaders {
    getHeadersVersion  :: !Word32,
    getHeadersBL       :: !BlockLocator,
    getHeadersHashStop :: !Hash256
} deriving (Eq, Show)

instance Binary GetHeaders where

    get = GetHeaders <$> getWord32le
                     <*> (repList =<< get)
                     <*> get
        where repList (VarInt c) = replicateM (fromIntegral c) get

    put (GetHeaders v xs h) = do
        putWord32le v
        put $ VarInt $ fromIntegral $ length xs
        forM_ xs put
        put h

