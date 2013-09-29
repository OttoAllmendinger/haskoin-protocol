module Haskoin.Protocol.Tests (tests) where

import Test.QuickCheck.Property hiding ((.&.))
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import QuickCheckUtils
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
        , testProperty "ScriptOutput" (metaGetPut :: ScriptOutput -> Bool)
        , testProperty "ScriptInput" (metaGetPut :: ScriptInput -> Bool)
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
    , testGroup "Scripts"
        [ testProperty "ScriptOutput from/to [ScriptOp]" scriptOutputOps
        , testProperty "parse PubKey Script" testParseInputPubKey
        , testProperty "parse PubKeyHash Script" testParseInputPubKeyHash
        , testProperty "parse Sig2 Script" testParseInputSig2
        , testProperty "parse Sig3 Script" testParseInputSig3
        , testProperty "parse ScriptHash Script" testParseInputScriptHash
        ]
        
    ]

metaGetPut :: (Binary a, Eq a) => a -> Bool
metaGetPut x = (runGet get (runPut $ put x)) == x

scriptOutputOps :: ScriptOutput -> Bool
scriptOutputOps so = (scriptOpsToOutput $ outputToScriptOps so) == so

testParseInputPubKey :: Signature -> Bool
testParseInputPubKey s = (fromJust $ parseInputPubKey $ spendPubKey s) == s

testParseInputPubKeyHash :: Signature -> PubKey -> Bool
testParseInputPubKeyHash s p = 
    (fromJust $ parseInputPubKeyHash $ spendPubKeyHash s p) == (s,p)

testParseInputSig2 :: Signature -> Signature -> Bool
testParseInputSig2 s1 s2 = 
    (fromJust $ parseInputSig2 $ spendSig2 s1 s2) == (s1,s2)

testParseInputSig3 :: Signature -> Signature -> Signature -> Bool
testParseInputSig3 s1 s2 s3 = 
    (fromJust $ parseInputSig3 $ spendSig3 s1 s2 s3) == (s1,s2,s3)

testParseInputScriptHash :: ScriptInput -> ScriptOutput -> Bool
testParseInputScriptHash si so = 
    (fromJust $ parseInputScriptHash $ spendScriptHash si so) == (si,so)

