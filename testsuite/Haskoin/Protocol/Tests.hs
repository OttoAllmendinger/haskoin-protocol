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
        , testProperty "parse PubKey Script" testParsePK
        , testProperty "parse PubKeyHash Script" testParsePKHash
        , testProperty "parse Sig2 Script" testParseSig2
        , testProperty "parse Sig3 Script" testParseSig3
        , testProperty "parse ScriptHash Script" testParseSHash
        ]
        
    ]

metaGetPut :: (Binary a, Eq a) => a -> Bool
metaGetPut x = (runGet get (runPut $ put x)) == x

scriptOutputOps :: ScriptOutput -> Bool
scriptOutputOps so = (scriptOpsToOutput $ outputToScriptOps so) == so

testParsePK :: Signature -> Bool
testParsePK s = (fromJust $ parsePK $ spendPK s) == s

testParsePKHash :: Signature -> PubKey -> Bool
testParsePKHash s p = 
    (fromJust $ parsePKHash $ spendPKHash s p) == (s,p)

testParseSig2 :: Signature -> Signature -> Bool
testParseSig2 s1 s2 = 
    (fromJust $ parseSig2 $ spendSig2 s1 s2) == (s1,s2)

testParseSig3 :: Signature -> Signature -> Signature -> Bool
testParseSig3 s1 s2 s3 = 
    (fromJust $ parseSig3 $ spendSig3 s1 s2 s3) == (s1,s2,s3)

testParseSHash :: ScriptInput -> ScriptOutput -> Bool
testParseSHash si so = 
    (fromJust $ parseSHash $ spendSHash si so) == (si,so)

