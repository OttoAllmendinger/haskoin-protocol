module Haskoin.Protocol.Headers 
( Headers(..)
, BlockHeaderCount
) where

import Control.Monad (liftM2, replicateM, forM_)
import Control.Applicative ((<$>))

import Data.Binary (Binary, get, put)

import Haskoin.Protocol.VarInt
import Haskoin.Protocol.BlockHeader

type BlockHeaderCount = (BlockHeader, VarInt)

data Headers = Headers {
    headersList :: ![BlockHeaderCount]
} deriving (Eq, Show)

instance Binary Headers where

    get = Headers <$> (repList =<< get)
        where repList (VarInt c) = replicateM (fromIntegral c) action
              action = liftM2 (,) get get

    put (Headers xs) = do
        put $ VarInt $ fromIntegral $ length xs
        forM_ xs $ \(a,b) -> put a >> put b

