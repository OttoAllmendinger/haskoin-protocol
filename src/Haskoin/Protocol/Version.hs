module Haskoin.Protocol.Version ( Version(..) ) where

import Control.Applicative ((<$>),(<*>))

import Data.Word (Word32, Word64)
import Data.Binary (Binary, get, put, Get, Put)
import Data.Binary.Get 
    ( getWord8
    , getWord32le
    , getWord64le
    , isEmpty
    )
import Data.Binary.Put 
    ( putWord8
    , putWord32le
    , putWord64le
    )

import Haskoin.Protocol.VarString
import Haskoin.Protocol.NetworkAddress

data Version = Version {
    version     :: !Word32,
    services    :: !Word64,
    timestamp   :: !Word64,
    addrRecv    :: !NetworkAddress,
    addrSend    :: !NetworkAddress,
    verNonce    :: !Word64,
    userAgent   :: !VarString,
    startHeight :: !Word32,
    relay       :: !Bool
} deriving (Eq, Show)

instance Binary Version where

    get = Version <$> getWord32le
                  <*> getWord64le
                  <*> getWord64le
                  <*> get
                  <*> get
                  <*> getWord64le
                  <*> get
                  <*> getWord32le
                  <*> (go =<< isEmpty)
        where go True  = return True
              go False = getBool

    put (Version v s t ar as n ua sh r) = do
        putWord32le v
        putWord64le s
        putWord64le t
        put         ar
        put         as
        putWord64le n
        put         ua
        putWord32le sh
        putBool     r

getBool :: Get Bool
getBool = go =<< getWord8
    where go 0 = return False
          go _ = return True

putBool :: Bool -> Put 
putBool True  = putWord8 1
putBool False = putWord8 0

