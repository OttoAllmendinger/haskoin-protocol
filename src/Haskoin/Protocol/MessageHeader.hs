module Haskoin.Protocol.MessageHeader 
    ( MessageHeader(..) 
    , MessageCommand(..)
    ) where

import Control.Applicative ((<$>),(<*>))

import Data.Char (ord, chr)
import Data.Word (Word32)
import qualified Data.ByteString as BS 
    ( ByteString
    , pack, unpack
    , takeWhile
    )
import Data.Binary (Binary, get, put)
import Data.Binary.Get 
    ( getWord32le
    , getWord32be
    , getByteString
    )
import Data.Binary.Put 
    ( putWord32le
    , putWord32be
    , putByteString
    )

import Haskoin.Util (stringToBS, bsToString)

data MessageCommand =
        MCAddr |
        MCAlert |
        MCBlockHeader |
        MCBlock |
        MCGetBlocks |
        MCGetData |
        MCGetHeaders |
        MCHeaders |
        MCInv |
        MCPing |
        MCTx |
        MCVersion
        deriving (Eq, Show)

instance Binary MessageCommand where
    
    get = go =<< getByteString 12
        where go bs = return $ case unpackCommand bs of
                "addr"        -> MCAddr
                "alert"       -> MCAlert
                "blockheader" -> MCBlockHeader
                "block"       -> MCBlock
                "getblocks"   -> MCGetBlocks
                "getdata"     -> MCGetData
                "getheaders"  -> MCGetHeaders
                "headers"     -> MCHeaders
                "inv"         -> MCInv
                "ping"        -> MCPing
                "tx"          -> MCTx
                "version"     -> MCVersion

    put mc = putByteString $ packCommand $ case mc of
        MCAddr        -> "addr"
        MCAlert       -> "alert"
        MCBlockHeader -> "blockheader"
        MCBlock       -> "block"
        MCGetBlocks   -> "getblocks"
        MCGetData     -> "getdata"
        MCGetHeaders  -> "getheaders"
        MCHeaders     -> "headers"
        MCInv         -> "inv"
        MCPing        -> "ping"
        MCTx          -> "tx"
        MCVersion     -> "version"

packCommand :: String -> BS.ByteString
packCommand s = stringToBS $ take 12 $ s ++ repeat '\NUL'

unpackCommand :: BS.ByteString -> String
unpackCommand bs = bsToString $ BS.takeWhile (/= 0) bs

data MessageHeader = MessageHeader {
    headMagic       :: !Word32,
    headCmd         :: !MessageCommand,
    headPayloadSize :: !Word32,
    headChecksum    :: !Word32
} deriving (Eq, Show)

instance Binary MessageHeader where

    get = MessageHeader <$> getWord32be
                        <*> get
                        <*> getWord32le
                        <*> getWord32be

    put (MessageHeader m c l cs) = do
        putWord32be m
        put         c
        putWord32le l
        putWord32be cs

