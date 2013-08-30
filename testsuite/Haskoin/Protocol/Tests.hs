module Haskoin.Protocol.Tests (tests) where

import Test.QuickCheck.Property hiding ((.&.))
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import QuickCheckUtils
import Haskoin.Protocol

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
    ]

metaGetPut :: (Binary a, Eq a) => a -> Bool
metaGetPut x = (runGet get (runPut $ put x)) == x

