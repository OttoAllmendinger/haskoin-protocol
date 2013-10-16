module Haskoin.Protocol.Tests (tests) where

import Test.QuickCheck.Property hiding ((.&.))
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Data.ByteString.Lazy as BL

import Haskoin.Protocol.Arbitrary
import Haskoin.Protocol
import Haskoin.Protocol.Script
import Haskoin.Crypto

tests :: [Test]
tests = 
    [ testGroup "Serialize & de-serialize protocol messages"
        [ testProperty "VarInt" (metaGetPut :: VarInt -> Bool)
        , testProperty "VarString" (metaGetPut :: VarString -> Bool)
        , testProperty "NetworkAddress" (metaGetPut :: NetworkAddress -> Bool)
        , testProperty "InvType" (metaGetPut :: InvType -> Bool)
        , testProperty "InvVector" (metaGetPut :: InvVector -> Bool)
        , testProperty "Inv" (metaGetPut :: Inv -> Bool)
        , testProperty "Version" (metaGetPut :: Version -> Bool)
        , testProperty "Addr" (metaGetPut :: Addr -> Bool)
        , testProperty "Alert" (metaGetPut :: Alert -> Bool)
        , testProperty "TxIn" (metaGetPut :: TxIn -> Bool)
        , testProperty "TxOut" (metaGetPut :: TxOut -> Bool)
        , testProperty "OutPoint" (metaGetPut :: OutPoint -> Bool)
        , testProperty "ScriptOp" (metaGetPut :: ScriptOp -> Bool)
        , testProperty "Script" (metaGetPut :: Script -> Bool)
        , testProperty "Tx" (metaGetPut :: Tx -> Bool)
        , testProperty "CoinbaseTx" (metaGetPut :: CoinbaseTx -> Bool)
        , testProperty "BlockHeader" (metaGetPut :: BlockHeader -> Bool)
        , testProperty "Block" (metaGetPut :: Block -> Bool)
        , testProperty "GetBlocks" (metaGetPut :: GetBlocks -> Bool)
        , testProperty "GetData" (metaGetPut :: GetData -> Bool)
        , testProperty "GetHeaders" (metaGetPut :: GetHeaders -> Bool)
        , testProperty "Headers" (metaGetPut :: Headers -> Bool)
        , testProperty "NotFound" (metaGetPut :: NotFound -> Bool)
        , testProperty "Ping" (metaGetPut :: Ping -> Bool)
        , testProperty "Pong" (metaGetPut :: Pong -> Bool)
        , testProperty "MessageCommand" (metaGetPut :: MessageCommand -> Bool)
        , testProperty "MessageHeader" (metaGetPut :: MessageHeader -> Bool)
        , testProperty "Message" (metaGetPut :: Message -> Bool)
        ]
    , testGroup "De-serialize & serialize arbitrary bytes into ScriptOps"
        [ testProperty "ScriptOp" testEncodeDecode ]
    ]

metaGetPut :: (Binary a, Eq a) => a -> Bool
metaGetPut x = (runGet get (runPut $ put x)) == x


testEncodeDecode :: Word8 -> Bool
testEncodeDecode x
    -- ignore constants
    | (x > 0) && (x < 0x4f) = True
    | x == 0xfe = True
    | x == 0xfd = True
    | otherwise = case encode x of
        -- ignore invalid opcodes
        OP_INVALIDOPCODE _ -> True
        -- decode . encode == id
        op -> decode x == x
        where encode x = runGet get $ BL.singleton x
              decode x = BL.head $ runPut $ put x
