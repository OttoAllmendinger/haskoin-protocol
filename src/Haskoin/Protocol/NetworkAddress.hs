module Haskoin.Protocol.NetworkAddress ( NetworkAddress(..) ) where

import Control.Monad (liftM2)
import Control.Applicative ((<$>),(<*>))

import Data.Word (Word16, Word64)
import Data.Binary (Binary, get, put)
import Data.Binary.Get 
    ( getWord16be
    , getWord64le
    , getWord64be
    )
import Data.Binary.Put
    ( putWord16be
    , putWord64le
    , putWord64be
    )

data NetworkAddress = NetworkAddress {
    naServices :: Word64,
    address  :: (Word64, Word64),
    port     :: Word16
} deriving (Eq, Show)

instance Binary NetworkAddress where

    get = NetworkAddress <$> getWord64le
                         <*> (liftM2 (,) getWord64be getWord64be)
                         <*> getWord16be

    put (NetworkAddress s (al,ar) p) = do
        putWord64le s
        putWord64be al
        putWord64be ar
        putWord16be p

