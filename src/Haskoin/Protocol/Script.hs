{-# LANGUAGE StandaloneDeriving, OverlappingInstances, DeriveDataTypeable #-}

module Haskoin.Protocol.Script 
( ScriptOp(..)
, Script(..)
, getScriptOps
, putScriptOps
) where

import Control.Monad (liftM2)
import Control.Applicative ((<$>), (<*>))

import Data.Data
import Data.Word (Word8)
import Data.Binary (Binary, get, put)
import Data.Binary.Get 
    ( Get
    , isEmpty
    , getWord8
    , getWord16le
    , getWord32le
    , getWord32be
    , getByteString
    )
import Data.Binary.Put 
    ( Put 
    , runPut
    , putWord8
    , putWord16le
    , putWord32le
    , putWord32be
    , putByteString
    )
import qualified Data.ByteString as BS
    ( ByteString
    , length
    )

import Haskoin.Protocol.VarInt
import Haskoin.Util 
    ( isolate
    , runPut'
    , encode'
    , bsToHex
    )
import Haskoin.Crypto 
    ( PubKey
    , pubKeyAddr
    , Hash160
    , hash160
    , hash256BS
    , Address(..)
    , Signature
    )

data Script = Script { runScript :: [ScriptOp] }
    deriving (Eq, Show)

instance Binary Script where
    get = do
        (VarInt len) <- get
        isolate (fromIntegral len) $ Script <$> getScriptOps

    put (Script ops) = do
        let bs = runPut' $ putScriptOps ops
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
    OP_0 |
    OP_1NEGATE |
    OP_1  | OP_2  | OP_3  | OP_4  |
    OP_5  | OP_6  | OP_7  | OP_8  |
    OP_9  | OP_10 | OP_11 | OP_12 |
    OP_13 | OP_14 | OP_15 | OP_16 |

    -- Flow control
    OP_NOP |
    OP_NOP1 | OP_NOP2 | OP_NOP3 | OP_NOP4 | OP_NOP5 |
    OP_NOP6 | OP_NOP7 | OP_NOP8 | OP_NOP9 | OP_NOP10 |

    OP_IF |
    OP_NOTIF |
    OP_ELSE |
    OP_ENDIF |
    OP_VERIFY |
    OP_RETURN |

    -- Stack operations
    OP_TOALTSTACK |
    OP_FROMALTSTACK |
    OP_IFDUP |
    OP_DEPTH |
    OP_DROP |
    OP_DUP |
    OP_NIP |
    OP_OVER |
    OP_PICK |
    OP_ROLL |
    OP_ROT |
    OP_SWAP |
    OP_TUCK |
    OP_2DROP |
    OP_2DUP |
    OP_3DUP |
    OP_2OVER |
    OP_2ROT |
    OP_2SWAP |

    -- Splice
    OP_CAT |
    OP_SUBSTR |
    OP_LEFT |
    OP_RIGHT |
    OP_SIZE |

    -- Bitwise logic
    OP_INVERT |
    OP_AND |
    OP_OR |
    OP_XOR |
    OP_EQUAL |
    OP_EQUALVERIFY |

    -- Arithmetic
    OP_1ADD |
    OP_1SUB |
    OP_2SUB |
    OP_2MUL |
    OP_2DIV |
    OP_NEGATE |
    OP_ABS |
    OP_NOT |
    OP_0NOTEQUAL |
    OP_ADD |
    OP_SUB |
    OP_MUL |
    OP_DIV |
    OP_MOD |
    OP_LSHIFT |
    OP_RSHIFT |
    OP_BOOLAND |
    OP_BOOLOR |
    OP_NUMEQUAL |
    OP_NUMEQUALVERIFY |
    OP_NUMNOTEQUAL |
    OP_LESSTHAN |
    OP_GREATERTHAN |
    OP_LESSTHANOREQUAL |
    OP_GREATERTHANOREQUAL |
    OP_MIN |
    OP_MAX |
    OP_WITHIN |

    -- Crypto
    OP_RIPEMD160 |
    OP_SHA1 |
    OP_SHA256 |
    OP_HASH160 |
    OP_HASH256 |
    OP_CODESEPARATOR |
    OP_CHECKSIG |
    OP_CHECKSIGVERIFY |
    OP_CHECKMULTISIG |
    OP_CHECKMULTISIGVERIFY |

    -- Other
    OP_PUBKEYHASH Hash160 |
    OP_PUBKEY PubKey |
    OP_INVALIDOPCODE Word8
        deriving (Eq, Show)


{-
-- making ScriptOp an instsance of Show is a little verbose
-- exploring better ways with generic-deriving:
-- http://hackage.haskell.org/package/generic-deriving

instance Show ScriptOp where
    show op = case op of
        (OP_PUSHDATA bs)     -> "OP_PUSHDATA " ++ (show $ bsToHex bs)
        (OP_PUBKEY p)        -> "OP_PUBKEY " ++ (show $ bsToHex $ encode' p)
        (OP_INVALIDOPCODE w) -> "OP_INVALIDOPCODE " ++ (show w)
-}

instance Binary ScriptOp where

    get = go =<< (fromIntegral <$> getWord8)
        where go 0x00 = return OP_0
              -- 0x01 ... 0x4e: constants, see at the end
              go 0x4f = return OP_1NEGATE
              go 0x51 = return OP_1
              go 0x52 = return OP_2
              go 0x53 = return OP_3
              go 0x54 = return OP_4
              go 0x55 = return OP_5
              go 0x56 = return OP_6
              go 0x57 = return OP_7
              go 0x58 = return OP_8
              go 0x59 = return OP_9
              go 0x5a = return OP_10
              go 0x5b = return OP_11
              go 0x5c = return OP_12
              go 0x5d = return OP_13
              go 0x5e = return OP_14
              go 0x5f = return OP_15
              go 0x60 = return OP_16


              -- Flow control
              go 0x61 = return OP_NOP
              -- go 0x62 = return OP_VER        -- reserved
              go 0x63 = return OP_IF
              go 0x64 = return OP_NOTIF
              -- go 0x65 = return OP_VERIF      -- reserved
              -- go 0x66 = return OP_VERNOTIF   -- reserved
              go 0x67 = return OP_ELSE
              go 0x68 = return OP_ENDIF
              go 0x69 = return OP_VERIFY
              go 0x6a = return OP_RETURN

              -- Stack
              go 0x6b = return OP_TOALTSTACK
              go 0x6c = return OP_FROMALTSTACK
              go 0x6d = return OP_2DROP
              go 0x6e = return OP_2DUP
              go 0x6f = return OP_3DUP
              go 0x70 = return OP_2OVER
              go 0x71 = return OP_2ROT
              go 0x72 = return OP_2SWAP
              go 0x73 = return OP_IFDUP
              go 0x74 = return OP_DEPTH
              go 0x75 = return OP_DROP
              go 0x76 = return OP_DUP
              go 0x77 = return OP_NIP
              go 0x78 = return OP_OVER
              go 0x79 = return OP_PICK
              go 0x7a = return OP_ROLL
              go 0x7b = return OP_ROT
              go 0x7c = return OP_SWAP
              go 0x7d = return OP_TUCK

              -- Splice
              go 0x7e = return OP_CAT
              go 0x7f = return OP_SUBSTR
              go 0x80 = return OP_LEFT
              go 0x81 = return OP_RIGHT
              go 0x82 = return OP_SIZE

              -- Bitwise logic
              go 0x83 = return OP_INVERT
              go 0x84 = return OP_AND
              go 0x85 = return OP_OR
              go 0x86 = return OP_XOR
              go 0x87 = return OP_EQUAL
              go 0x88 = return OP_EQUALVERIFY
              -- go 0x89 = return OP_RESERVED1
              -- go 0x8a = return OP_RESERVED2

              -- Arithmetic
              go 0x8b = return OP_1ADD
              go 0x8c = return OP_1SUB
              go 0x8d = return OP_2MUL
              go 0x8e = return OP_2DIV
              go 0x8f = return OP_NEGATE
              go 0x90 = return OP_ABS
              go 0x91 = return OP_NOT
              go 0x92 = return OP_0NOTEQUAL
              go 0x93 = return OP_ADD
              go 0x94 = return OP_SUB
              go 0x95 = return OP_MUL
              go 0x96 = return OP_DIV
              go 0x97 = return OP_MOD
              go 0x98 = return OP_LSHIFT
              go 0x99 = return OP_RSHIFT
              go 0x9a = return OP_BOOLAND
              go 0x9b = return OP_BOOLOR
              go 0x9c = return OP_NUMEQUAL
              go 0x9d = return OP_NUMEQUALVERIFY
              go 0x9e = return OP_NUMNOTEQUAL
              go 0x9f = return OP_LESSTHAN
              go 0xa0 = return OP_GREATERTHAN
              go 0xa1 = return OP_LESSTHANOREQUAL
              go 0xa2 = return OP_GREATERTHANOREQUAL
              go 0xa3 = return OP_MIN
              go 0xa4 = return OP_MAX
              go 0xa5 = return OP_WITHIN

              -- Crypto
              go 0xa6 = return OP_RIPEMD160
              go 0xa7 = return OP_SHA1
              go 0xa8 = return OP_SHA256
              go 0xa9 = return OP_HASH160
              go 0xaa = return OP_HASH256
              go 0xab = return OP_CODESEPARATOR
              go 0xac = return OP_CHECKSIG
              go 0xad = return OP_CHECKSIGVERIFY
              go 0xae = return OP_CHECKMULTISIG
              go 0xaf = return OP_CHECKMULTISIGVERIFY

              -- More NOPs
              go 0xb0 = return OP_NOP1
              go 0xb1 = return OP_NOP2
              go 0xb2 = return OP_NOP3
              go 0xb3 = return OP_NOP4
              go 0xb4 = return OP_NOP5
              go 0xb5 = return OP_NOP6
              go 0xb6 = return OP_NOP7
              go 0xb7 = return OP_NOP8
              go 0xb8 = return OP_NOP9
              go 0xb9 = return OP_NOP10

              -- Constants
              go 0xfd = OP_PUBKEYHASH <$> get
              go 0xfe = OP_PUBKEY <$> get
              go op | op <= 0x4b = do
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
                    -- Invalid Opcode
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

        -- Constants
        OP_0                 -> putWord8 0x00
        OP_1NEGATE           -> putWord8 0x4f
        OP_1                 -> putWord8 0x51
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

        -- Crypto Constants
        (OP_PUBKEY pk)       -> putWord8 0xfe >> put pk

        -- Invalid Opcodes (maybe output the actual opcode?)
        (OP_INVALIDOPCODE _) -> putWord8 0xff

        -- Flow Control
        OP_NOP               -> putWord8 0x61
        -- OP_VER            -> putWord8 0x62
        OP_IF                -> putWord8 0x63
        OP_NOTIF             -> putWord8 0x64
        -- OP_VERIF          -> putWord8 0x65
        -- OP_VERNOTIF       -> putWord8 0x66
        OP_ELSE              -> putWord8 0x67
        OP_ENDIF             -> putWord8 0x68
        OP_VERIFY            -> putWord8 0x69
        OP_RETURN            -> putWord8 0x6a

        -- Stack Operations
        OP_TOALTSTACK        -> putWord8 0x6b
        OP_FROMALTSTACK      -> putWord8 0x6c
        OP_2DROP             -> putWord8 0x6d
        OP_2DUP              -> putWord8 0x6e
        OP_3DUP              -> putWord8 0x6f
        OP_2OVER             -> putWord8 0x70
        OP_2ROT              -> putWord8 0x71
        OP_2SWAP             -> putWord8 0x72
        OP_IFDUP             -> putWord8 0x73
        OP_DEPTH             -> putWord8 0x74
        OP_DROP              -> putWord8 0x75
        OP_DUP               -> putWord8 0x76
        OP_NIP               -> putWord8 0x77
        OP_OVER              -> putWord8 0x78
        OP_PICK              -> putWord8 0x79
        OP_ROLL              -> putWord8 0x7a
        OP_ROT               -> putWord8 0x7b
        OP_SWAP              -> putWord8 0x7c
        OP_TUCK              -> putWord8 0x7d

        -- Splice
        OP_CAT               -> putWord8 0x7e
        OP_SUBSTR            -> putWord8 0x7f
        OP_LEFT              -> putWord8 0x80
        OP_RIGHT             -> putWord8 0x81
        OP_SIZE              -> putWord8 0x82

        -- Bitwise Logic
        OP_INVERT            -> putWord8 0x83
        OP_AND               -> putWord8 0x84
        OP_OR                -> putWord8 0x85
        OP_XOR               -> putWord8 0x86
        OP_EQUAL             -> putWord8 0x87
        OP_EQUALVERIFY       -> putWord8 0x88

        -- Arithmetic
        OP_1ADD              -> putWord8 0x8b
        OP_1SUB              -> putWord8 0x8c
        OP_2MUL              -> putWord8 0x8d
        OP_2DIV              -> putWord8 0x8e
        OP_NEGATE            -> putWord8 0x8f
        OP_ABS               -> putWord8 0x90
        OP_NOT               -> putWord8 0x91
        OP_0NOTEQUAL         -> putWord8 0x92
        OP_ADD               -> putWord8 0x93
        OP_SUB               -> putWord8 0x94
        OP_MUL               -> putWord8 0x95
        OP_DIV               -> putWord8 0x96
        OP_MOD               -> putWord8 0x97
        OP_LSHIFT            -> putWord8 0x98
        OP_RSHIFT            -> putWord8 0x99
        OP_BOOLAND           -> putWord8 0x9a
        OP_BOOLOR            -> putWord8 0x9b
        OP_NUMEQUAL          -> putWord8 0x9c
        OP_NUMEQUALVERIFY    -> putWord8 0x9d
        OP_NUMNOTEQUAL       -> putWord8 0x9e
        OP_LESSTHAN          -> putWord8 0x9f
        OP_GREATERTHAN       -> putWord8 0xa0
        OP_LESSTHANOREQUAL   -> putWord8 0xa1
        OP_GREATERTHANOREQUAL-> putWord8 0xa2
        OP_MIN               -> putWord8 0xa3
        OP_MAX               -> putWord8 0xa4
        OP_WITHIN            -> putWord8 0xa5

        -- Crypto
        OP_RIPEMD160         -> putWord8 0xa6
        OP_SHA1              -> putWord8 0xa7
        OP_SHA256            -> putWord8 0xa8
        OP_HASH160           -> putWord8 0xa9
        OP_HASH256           -> putWord8 0xaa
        OP_CODESEPARATOR     -> putWord8 0xab
        OP_CHECKSIG          -> putWord8 0xac
        OP_CHECKSIGVERIFY    -> putWord8 0xad
        OP_CHECKMULTISIG     -> putWord8 0xae
        OP_CHECKMULTISIGVERIFY -> putWord8 0xaf

        -- More NOPs
        OP_NOP1              -> putWord8 0xb0
        OP_NOP2              -> putWord8 0xb1
        OP_NOP3              -> putWord8 0xb2
        OP_NOP4              -> putWord8 0xb3
        OP_NOP5              -> putWord8 0xb4
        OP_NOP6              -> putWord8 0xb5
        OP_NOP7              -> putWord8 0xb6
        OP_NOP8              -> putWord8 0xb7
        OP_NOP9              -> putWord8 0xb8
        OP_NOP10             -> putWord8 0xb9
