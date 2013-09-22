module Haskoin.Protocol.Script 
( ScriptOp(..)
, Script(..)
) where

import Control.Monad (liftM2)
import Control.Applicative ((<$>))

import Data.Word (Word8)
import Data.Binary (Binary, get, put)
import Data.Binary.Get 
    ( Get
    , isEmpty
    , getWord8
    , getWord16le
    , getWord32le
    , getByteString
    )
import Data.Binary.Put 
    ( Put 
    , runPut
    , putWord8
    , putWord16le
    , putWord32le
    , putByteString
    )
import qualified Data.ByteString as BS
    ( ByteString
    , length
    )

import Haskoin.Protocol.VarInt
import Haskoin.Util (isolate, toStrictBS)
import Haskoin.Crypto (PubKey)

data MulSig2Type = OneOfTwo
                 | TwoOfTwo
                 deriving (Eq, Show)

data MulSig3Type = OneOfThree
                 | TwoOfThree
                 | ThreeOfThree
                 deriving (Eq, Show)

data ScriptOutput = PayPubKey 
                      { runPayPubKey     :: PubKey }
                  | PayPubKeyHash 
                      { runPayPubKeyHash :: Hash160 }
                  | PayMulSig1 
                      { runPayMulSig1    :: PubKey }
                  | PayMulSig2
                      { mulSig2Type      :: MulSig2Type
                      , fstMulSigKey     :: PubKey
                      , sndMulSigKey     :: PubKey
                      }
                  | PayMulSig3
                      { mulSig3Type      :: MulSig3Type
                      , fstMulSigKey     :: PubKey
                      , sndMulSigKey     :: PubKey
                      , trdMulSigKey     :: PubKey
                      }
                  | PayScriptHash 
                      { runPayScriptHash :: Hash160 }
                  | PayNonStd 
                      { runPayNonStd     :: [ScriptOp] }
                  deriving (Eq, Show)

opsToScriptOutput :: [ScriptOp] -> ScriptOutput
opsToScriptOutput ops = case ops of
    [OP_PUSHDATA k, OP_CHECKSIG] 
        -> either def (PayPubKey . lst) $ decodeOrFail' k
    [OP_DUP, OP_HASH160, OP_PUSHDATA h, OP_EQUALVERIFY, OP_CHECKSIG] 
        -> either def (PayPubKeyHash . lst) $ decodeOrFail' k
    [OP_1, OP_PUSHDATA k, OP_1, OP_CHECKMULTISIG] 
        -> either def (PayMulSig1 . lst) $ decodeOrFail' k
    [t, OP_PUSHDATA k1, OP_PUSHDATA k2, OP_2, OP_CHECKMULTISIG]
        -> either def id $ do
               (_,_,r1) <- decodeOrFail' k1
               (_,_,r2) <- decodeOrFail' k2
               return $ case t of 
                   OP_1 -> PayMulSig2 OneOfTwo r1 r2
                   OP_2 -> PayMulSig2 TwoOfTwo r1 r2
                    _    -> PayNonStd ops
    [t, OP_PUSHDATA k1, OP_PUSHDATA k2, OP_2, OP_CHECKMULTISIG]
        -> either def id $ do
               (_,_,r1) <- decodeOrFail' k1
               (_,_,r2) <- decodeOrFail' k2
               (_,_,r3) <- decodeOrFail' k3
               return $ case t of 
                   OP_1 -> PayMulSig3 OneOfThree   r1 r2 r3
                   OP_2 -> PayMulSig3 TwoOfThree   r1 r2 r3
                   OP_3 -> PayMulSig3 ThreeOfThree r1 r2 r3
                   _    -> PayNonStd ops
    [OP_HASH160, OP_PUSHDATA h, OP_EQUAL]
        -> either def (PayScriptHash . lst) $ decodeOrFail' k
    _ -> PayNonStd ops
    where def         = const $ PayNonStd ops
          lst (a,b,c) = c
    

data ScriptInput = SpendPubKey 
                     { runSpendPubKey     :: Signature }
                 | SpendPubKeyHash 
                     { sigSpendPubKeyHash :: Signature
                     , keySpendPubKeyHash :: PubKey
                     }
                 | SpendMulSig1
                     { runSpendMulSig1    :: Signature }
                 | SpendMulSig2
                     { fstSpendMulSig     :: Signature
                     , sndSpendMulSig     :: Signature
                     }
                 | SpendMulSig3
                     { fstSpendMulSig     :: Signature
                     , sndSpendMulSig     :: Signature
                     , trdSpendMulSig     :: Signature
                     }
                 | SpendScriptHash 
                     { inSpendScriptHash  :: ScriptInput
                     , outSpendScriptHash :: ScriptOutput
                     }
                 | SpendNonStd 
                     { runSpendNonStd     :: [ScriptOp] }
                 deriving (Eq, Show)

instance Binary ScriptOutput where

    get = do
        (VarInt len) <- get
        isolate (fromIntegral len) $ opsToScriptOutput <$> getScriptOps

    put s = do
        

instance Binary Script where

    get = do
        (VarInt len) <- get
        isolate (fromIntegral len) (Script <$> getScriptOps)

    put (Script xs) = do
        let bs = toStrictBS $ runPut (putScriptOps xs)
        put $ VarInt $ fromIntegral $ BS.length bs
        putByteString bs

getScriptOps :: Get [ScriptOp]
getScriptOps = do
    empty <- isEmpty
    if empty 
        then return [] 
        else liftM2 (:) get getScriptOps

putScriptOps :: [ScriptOp] -> Put
putScriptOps (x:xs) = put x >> putScriptOps xs
putScriptOps _       = return ()

data ScriptOp =

    -- Pushing Data
    OP_PUSHDATA BS.ByteString |
    OP_FALSE | 
    OP_1NEGATE | 
    OP_TRUE |
    OP_2  | OP_3  | OP_4  | OP_5  | OP_6  | 
    OP_7  | OP_8  | OP_9  | OP_10 | OP_11 | 
    OP_12 | OP_13 | OP_14 | OP_15 | OP_16 |

    -- Flow control
    OP_VERIFY |

    -- Stack operations
    OP_DUP |

    -- Bitwise logic
    OP_EQUAL |
    OP_EQUALVERIFY | 

    -- Crypto
    OP_HASH160 |
    OP_CHECKSIG |
    OP_CHECKMULTISIG |

    -- Other
    OP_PUBKEY PubKey |
    OP_INVALIDOPCODE Word8
        deriving (Eq, Show)

instance Binary ScriptOp where

    get = go =<< (fromIntegral <$> getWord8) 
        where go op | op == 0x00 = return $ OP_FALSE
                    | op <= 0x4b = do
                        payload <- getByteString (fromIntegral op)
                        return $ OP_PUSHDATA payload
                    | op == 0x4c = do
                        len  <- getWord8
                        payload <- getByteString (fromIntegral len)
                        return $ OP_PUSHDATA payload
                    | op == 0x4d = do
                        len  <- getWord16le
                        payload <- getByteString (fromIntegral len)
                        return $ OP_PUSHDATA payload
                    | op == 0x4e = do
                        len  <- getWord32le
                        payload <- getByteString (fromIntegral len)
                        return $ OP_PUSHDATA payload
                    | op == 0x4f = return $ OP_1NEGATE
                    | op == 0x51 = return $ OP_TRUE
                    | op == 0x52 = return $ OP_2
                    | op == 0x53 = return $ OP_3
                    | op == 0x54 = return $ OP_4
                    | op == 0x55 = return $ OP_5
                    | op == 0x56 = return $ OP_6
                    | op == 0x57 = return $ OP_7
                    | op == 0x58 = return $ OP_8
                    | op == 0x59 = return $ OP_9
                    | op == 0x5a = return $ OP_10
                    | op == 0x5b = return $ OP_11
                    | op == 0x5c = return $ OP_12
                    | op == 0x5d = return $ OP_13
                    | op == 0x5e = return $ OP_14
                    | op == 0x5f = return $ OP_15
                    | op == 0x60 = return $ OP_16
                    | op == 0x69 = return $ OP_VERIFY
                    | op == 0x76 = return $ OP_DUP
                    | op == 0x87 = return $ OP_EQUAL
                    | op == 0x88 = return $ OP_EQUALVERIFY
                    | op == 0xa9 = return $ OP_HASH160
                    | op == 0xac = return $ OP_CHECKSIG
                    | op == 0xae = return $ OP_CHECKMULTISIG
                    | op == 0xfe = OP_PUBKEY <$> get
                    | otherwise = return $ OP_INVALIDOPCODE op

    put op = case op of

        (OP_PUSHDATA payload) -> go payload (BS.length payload)
            where go p len 
                    | len <= 0 = fail "OP_PUSHDATA: data length must be > 0"
                    | len <= 0x4b = do
                        putWord8 $ fromIntegral len
                        putByteString p
                    | len <= 0xff = do
                        putWord8 0x4c
                        putWord8 $ fromIntegral len
                        putByteString p
                    | len <= 0xffff = do
                        putWord8 0x4d
                        putWord16le $ fromIntegral len
                        putByteString p
                    | len <= 0xffffffff = do
                        putWord8 0x4e
                        putWord32le $ fromIntegral len
                        putByteString p
                    | otherwise = 
                        fail "bitcoinPut OP_PUSHDATA payload too big"

        OP_FALSE             -> putWord8 0x00
        OP_1NEGATE           -> putWord8 0x4f
        OP_TRUE              -> putWord8 0x51
        OP_2                 -> putWord8 0x52
        OP_3                 -> putWord8 0x53
        OP_4                 -> putWord8 0x54
        OP_5                 -> putWord8 0x55
        OP_6                 -> putWord8 0x56
        OP_7                 -> putWord8 0x57
        OP_8                 -> putWord8 0x58
        OP_9                 -> putWord8 0x59
        OP_10                -> putWord8 0x5a
        OP_11                -> putWord8 0x5b
        OP_12                -> putWord8 0x5c
        OP_13                -> putWord8 0x5d
        OP_14                -> putWord8 0x5e
        OP_15                -> putWord8 0x5f
        OP_16                -> putWord8 0x60
        OP_VERIFY            -> putWord8 0x69
        OP_DUP               -> putWord8 0x76
        OP_EQUAL             -> putWord8 0x87
        OP_EQUALVERIFY       -> putWord8 0x88
        OP_HASH160           -> putWord8 0xa9
        OP_CHECKSIG          -> putWord8 0xac
        OP_CHECKMULTISIG     -> putWord8 0xae
        (OP_PUBKEY pk)       -> putWord8 0xfe >> put pk
        (OP_INVALIDOPCODE _) -> putWord8 0xff

