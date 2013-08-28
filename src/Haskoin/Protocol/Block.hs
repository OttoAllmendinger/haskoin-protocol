module Haskoin.Protocol.Block ( Block(..) ) where

import Control.Monad (replicateM, forM_)
import Control.Applicative ((<$>))

import Data.Binary (Binary, get, put)

import Haskoin.Protocol.Tx
import Haskoin.Protocol.VarInt
import Haskoin.Protocol.BlockHeader

data Block = Block {
    blockHeader     :: !BlockHeader,
    blockCoinbaseTx :: !CoinbaseTx,
    blockTxns       :: ![Tx]
} deriving (Eq, Show)

instance Binary Block where

    get = do
        head       <- get
        (VarInt c) <- get
        cb         <- get
        txs        <- replicateM (fromIntegral (c-1)) get
        return $ Block head cb txs

    put (Block h cb txs) = do
        put h
        put $ VarInt $ fromIntegral $ (length txs) + 1
        put cb
        forM_ txs put

