module QuickCheckUtils where

import Test.QuickCheck

import Control.Monad
import Control.Applicative 

import qualified Data.ByteString as BS

import Haskoin.Protocol
import Haskoin.Crypto (Hash256, hash256)

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
                            
instance Arbitrary Tx where
    arbitrary = do
        v   <- arbitrary
        tin <- do 
            l <- choose (0,10)
            vectorOf l arbitrary
        tout <- do
            l <- choose (0,50)
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
    arbitrary = TxOut <$> arbitrary
                      <*> arbitrary

instance Arbitrary OutPoint where
    arbitrary = OutPoint <$> (hash256 <$> arbitrary)
                         <*> arbitrary

instance Arbitrary Block where
    arbitrary = do
        h <- arbitrary
        c <- arbitrary
        t <- do 
            l <- choose (0,20)
            vectorOf l arbitrary
        return $ Block h c t

instance Arbitrary ScriptOp where
    arbitrary = oneof [ OP_PUSHDATA <$> nonEmptyByteString
                      , return OP_FALSE
                      ]

instance Arbitrary Script where
    arbitrary = Script <$> listOf arbitrary

-- from Data.ByteString project
instance Arbitrary BS.ByteString where
    arbitrary = do
        bs <- BS.pack `fmap` arbitrary
        n <- choose (0, 2)
        return (BS.drop n bs) -- to give us some with non-0 offset

nonEmptyByteString :: Gen BS.ByteString
nonEmptyByteString = do
    bs <- arbitrary
    return $ if BS.null bs then BS.pack [0] else bs

