module Haskoin.Protocol.GetData ( GetData(..) ) where

import Control.Monad (replicateM, forM_)
import Control.Applicative ((<$>))

import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)

import Haskoin.Protocol.InvVector
import Haskoin.Protocol.VarInt

data GetData = GetData {
   getDataList :: ![InvVector] 
} deriving (Eq, Show)

instance Binary GetData where

    get = GetData <$> (repList =<< get)
        where repList (VarInt c) = replicateM (fromIntegral c) get

    put (GetData xs) = do
        put $ VarInt $ fromIntegral $ length xs
        forM_ xs put

