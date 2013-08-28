module Haskoin.Protocol.NotFound ( NotFound(..) ) where

import Control.Monad (replicateM, forM_)
import Control.Applicative ((<$>))

import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)

import Haskoin.Protocol.VarInt
import Haskoin.Protocol.InvVector

data NotFound = NotFound {
   notFoundList :: ![InvVector] 
} deriving (Eq, Show)

instance Binary NotFound where

    get = NotFound <$> (repList =<< get)
        where repList (VarInt c) = replicateM (fromIntegral c) get

    put (NotFound xs) = do
        put $ VarInt $ fromIntegral $ length xs
        forM_ xs put

