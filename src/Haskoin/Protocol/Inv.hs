module Haskoin.Protocol.Inv ( Inv(..) ) where

import Control.Monad (replicateM, forM_)
import Control.Applicative ((<$>))

import Data.Binary (Binary, get, put)

import Haskoin.Protocol.InvVector
import Haskoin.Protocol.VarInt

data Inv = Inv {
   invList :: ![InvVector] 
} deriving (Eq, Show)

instance Binary Inv where

    get = Inv <$> (repList =<< get)
        where repList (VarInt c) = replicateM (fromIntegral c) get

    put (Inv xs) = do
        put $ VarInt $ fromIntegral $ length xs
        forM_ xs put

