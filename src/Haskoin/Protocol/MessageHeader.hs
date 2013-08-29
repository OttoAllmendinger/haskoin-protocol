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
import Haskoin.Crypto (CheckSum32)

data MessageCommand =
        MCVersion |
        MCVerAck |
        MCAddr |
        MCInv |
        MCGetData |
        MCNotFound |
        MCGetBlocks |
        MCGetHeaders |
        MCTx |
        MCBlock |
        MCHeaders |
        MCGetAddr |
        MCPing |
        MCPong |
        MCAlert
        deriving (Eq, Show)

instance Binary MessageCommand where
    
    get = go =<< getByteString 12
        where go bs = case unpackCommand bs of
                "version"    -> return MCVersion
                "verack"     -> return MCVerAck
                "addr"       -> return MCAddr
                "inv"        -> return MCInv
                "getdata"    -> return MCGetData
                "notfound"   -> return MCNotFound
                "getblocks"  -> return MCGetBlocks
                "getheaders" -> return MCGetHeaders
                "tx"         -> return MCTx
                "block"      -> return MCBlock
                "headers"    -> return MCHeaders
                "getaddr"    -> return MCGetAddr
                "ping"       -> return MCPing
                "pong"       -> return MCPong
                "alert"      -> return MCAlert
                _            -> fail "get MessageCommand : Invalid command"

    put mc = putByteString $ packCommand $ case mc of
        MCVersion     -> "version"
        MCVerAck      -> "verack"
        MCAddr        -> "addr"
        MCInv         -> "inv"
        MCGetData     -> "getdata"
        MCNotFound    -> "notfound"
        MCGetBlocks   -> "getblocks"
        MCGetHeaders  -> "getheaders"
        MCTx          -> "tx"
        MCBlock       -> "block"
        MCHeaders     -> "headers"
        MCGetAddr     -> "getaddr"
        MCPing        -> "ping"
        MCPong        -> "pong"
        MCAlert       -> "alert"

packCommand :: String -> BS.ByteString
packCommand s = stringToBS $ take 12 $ s ++ repeat '\NUL'

unpackCommand :: BS.ByteString -> String
unpackCommand bs = bsToString $ BS.takeWhile (/= 0) bs

data MessageHeader = MessageHeader {
    headMagic       :: !Word32,
    headCmd         :: !MessageCommand,
    headPayloadSize :: !Word32,
    headChecksum    :: !CheckSum32
} deriving (Eq, Show)

instance Binary MessageHeader where

    get = MessageHeader <$> getWord32be
                        <*> get
                        <*> getWord32le
                        <*> get

    put (MessageHeader m c l chk) = do
        putWord32be m
        put         c
        putWord32le l
        put         chk

