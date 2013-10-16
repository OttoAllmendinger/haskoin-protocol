module Haskoin.Protocol.Arbitrary () where

import Test.QuickCheck
import Haskoin.Util.Arbitrary (nonEmptyBS)
import Haskoin.Crypto.Arbitrary

import Control.Monad
import Control.Applicative 

import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util 

instance Arbitrary VarInt where
    arbitrary = VarInt <$> arbitrary

instance Arbitrary VarString where
    arbitrary = VarString <$> arbitrary

instance Arbitrary NetworkAddress where
    arbitrary = do
        s <- arbitrary
        a <- liftM2 (,) arbitrary arbitrary
        p <- arbitrary
        return $ NetworkAddress s a p

instance Arbitrary InvType where
    arbitrary = elements [InvError, InvTx, InvBlock]

instance Arbitrary InvVector where
    arbitrary = InvVector <$> arbitrary <*> (hash256 <$> arbitrary)

instance Arbitrary Inv where
    arbitrary = Inv <$> listOf arbitrary

instance Arbitrary Version where
    arbitrary = Version <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary

instance Arbitrary Addr where
    arbitrary = Addr <$> listOf arbitrary

instance Arbitrary Alert where
    arbitrary = Alert <$> arbitrary <*> arbitrary

instance Arbitrary BlockHeader where
    arbitrary = BlockHeader <$> arbitrary
                            <*> (hash256 <$> arbitrary)
                            <*> (hash256 <$> arbitrary)
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            
instance Arbitrary Script where
    arbitrary = do
        i <- choose (1,10)
        Script <$> (vectorOf i arbitrary)

instance Arbitrary Tx where
    arbitrary = do
        v   <- arbitrary
        tin <- do 
            l <- choose (0,10)
            vectorOf l arbitrary
        tout <- do
            l <- choose (0,10)
            vectorOf l arbitrary
        t    <- arbitrary
        return $ Tx v tin tout t

instance Arbitrary CoinbaseTx where
    arbitrary = CoinbaseTx <$> arbitrary
                           <*> arbitrary
                           <*> (listOf arbitrary)
                           <*> arbitrary

instance Arbitrary TxIn where
    arbitrary = TxIn <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary

instance Arbitrary TxOut where
    arbitrary = TxOut <$> (choose (0,2100000000000000))
                      <*> arbitrary

instance Arbitrary OutPoint where
    arbitrary = OutPoint <$> arbitrary
                         <*> (choose (0,2147483647))

instance Arbitrary Block where
    arbitrary = do
        h <- arbitrary
        c <- arbitrary
        t <- do 
            l <- choose (0,10)
            vectorOf l arbitrary
        return $ Block h c t

instance Arbitrary ScriptOp where
    arbitrary = oneof [ OP_PUSHDATA <$> nonEmptyBS
                      , return OP_0
                      , return OP_1NEGATE
                      , return OP_1
                      , return OP_2
                      , return OP_3
                      , return OP_4
                      , return OP_5
                      , return OP_6
                      , return OP_7
                      , return OP_8
                      , return OP_9
                      , return OP_10
                      , return OP_11
                      , return OP_12
                      , return OP_13
                      , return OP_14
                      , return OP_15
                      , return OP_16

                      -- Flow control
                      , return OP_NOP
                      , return OP_IF
                      , return OP_NOTIF
                      , return OP_ELSE
                      , return OP_ENDIF
                      , return OP_VERIFY
                      , return OP_RETURN

                      -- Stack
                      , return OP_TOALTSTACK
                      , return OP_FROMALTSTACK
                      , return OP_2DROP
                      , return OP_2DUP
                      , return OP_3DUP
                      , return OP_2OVER
                      , return OP_2ROT
                      , return OP_2SWAP
                      , return OP_IFDUP
                      , return OP_DEPTH
                      , return OP_DROP
                      , return OP_DUP
                      , return OP_NIP
                      , return OP_OVER
                      , return OP_PICK
                      , return OP_ROLL
                      , return OP_ROT
                      , return OP_SWAP
                      , return OP_TUCK

                      -- Splice
                      , return OP_CAT
                      , return OP_SUBSTR
                      , return OP_LEFT
                      , return OP_RIGHT
                      , return OP_SIZE

                      -- Bitwise logic
                      , return OP_INVERT
                      , return OP_AND
                      , return OP_OR
                      , return OP_XOR
                      , return OP_EQUAL
                      , return OP_EQUALVERIFY

                      -- Arithmetic
                      , return OP_1ADD
                      , return OP_1SUB
                      , return OP_2MUL
                      , return OP_2DIV
                      , return OP_NEGATE
                      , return OP_ABS
                      , return OP_NOT
                      , return OP_0NOTEQUAL
                      , return OP_ADD
                      , return OP_SUB
                      , return OP_MUL
                      , return OP_DIV
                      , return OP_MOD
                      , return OP_LSHIFT
                      , return OP_RSHIFT
                      , return OP_BOOLAND
                      , return OP_BOOLOR
                      , return OP_NUMEQUAL
                      , return OP_NUMEQUALVERIFY
                      , return OP_NUMNOTEQUAL
                      , return OP_LESSTHAN
                      , return OP_GREATERTHAN
                      , return OP_LESSTHANOREQUAL
                      , return OP_GREATERTHANOREQUAL
                      , return OP_MIN
                      , return OP_MAX
                      , return OP_WITHIN

                      -- Crypto
                      , return OP_RIPEMD160
                      , return OP_SHA1
                      , return OP_SHA256
                      , return OP_HASH160
                      , return OP_HASH256
                      , return OP_CODESEPARATOR
                      , return OP_CHECKSIG
                      , return OP_CHECKSIGVERIFY
                      , return OP_CHECKMULTISIG
                      , return OP_CHECKMULTISIGVERIFY

                      -- More NOPs
                      , return OP_NOP1
                      , return OP_NOP2
                      , return OP_NOP3
                      , return OP_NOP4
                      , return OP_NOP5
                      , return OP_NOP6
                      , return OP_NOP7
                      , return OP_NOP8
                      , return OP_NOP9
                      , return OP_NOP10
                      , OP_PUBKEY <$> arbitrary
                      , return $ OP_INVALIDOPCODE 0xff
                      ]

instance Arbitrary GetBlocks where
    arbitrary = GetBlocks <$> arbitrary
                          <*> (listOf arbitrary)
                          <*> arbitrary

instance Arbitrary GetData where
    arbitrary = GetData <$> (listOf arbitrary)

instance Arbitrary GetHeaders where
    arbitrary = GetHeaders <$> arbitrary
                           <*> (listOf arbitrary)
                           <*> arbitrary

instance Arbitrary Headers where
    arbitrary = Headers <$> (listOf (liftM2 (,) arbitrary arbitrary))

instance Arbitrary NotFound where
    arbitrary = NotFound <$> (listOf arbitrary)

instance Arbitrary Ping where
    arbitrary = Ping <$> arbitrary

instance Arbitrary Pong where
    arbitrary = Pong <$> arbitrary

instance Arbitrary MessageCommand where
    arbitrary = elements [ MCVersion
                         , MCVerAck
                         , MCAddr
                         , MCInv
                         , MCGetData
                         , MCNotFound
                         , MCGetBlocks
                         , MCGetHeaders
                         , MCTx
                         , MCBlock
                         , MCHeaders
                         , MCGetAddr
                         , MCPing
                         , MCPong
                         , MCAlert
                         ]

instance Arbitrary MessageHeader where
    arbitrary = MessageHeader <$> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary

instance Arbitrary Message where
    arbitrary = oneof [ MVersion    <$> arbitrary
                      , return MVerAck
                      , MAddr       <$> arbitrary
                      , MInv        <$> arbitrary
                      , MGetData    <$> arbitrary
                      , MNotFound   <$> arbitrary
                      , MGetBlocks  <$> arbitrary
                      , MGetHeaders <$> arbitrary
                      , MTx         <$> arbitrary
                      , MBlock      <$> arbitrary
                      , MHeaders    <$> arbitrary
                      , return MGetAddr
                      , MPing       <$> arbitrary
                      , MPong       <$> arbitrary
                      , MAlert      <$> arbitrary
                      ]

