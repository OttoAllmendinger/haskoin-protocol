module Haskoin.Protocol.Message ( Message(..) ) where

import Control.Monad (unless)
import Control.Applicative ((<$>))

import Data.Word (Word32)
import Data.Binary (Binary, encode, get, put)
import Data.Binary.Get 
    ( lookAhead
    , getByteString
    )
import Data.Binary.Put (putByteString)
import qualified Data.ByteString as BS 
    ( length 
    , append
    , empty
    )

import Haskoin.Protocol.MessageHeader
import Haskoin.Protocol.Version
import Haskoin.Protocol.Addr
import Haskoin.Protocol.Inv
import Haskoin.Protocol.GetData
import Haskoin.Protocol.NotFound
import Haskoin.Protocol.GetBlocks
import Haskoin.Protocol.GetHeaders
import Haskoin.Protocol.Tx
import Haskoin.Protocol.Block
import Haskoin.Protocol.Headers
import Haskoin.Protocol.Ping
import Haskoin.Protocol.Alert

import Haskoin.Util (isolate, encode')
import Haskoin.Crypto (chksum32)

networkMagic :: Word32
networkMagic = 0xf9beb4d9

data Message = 
    MVersion Version | 
    MVerAck | 
    MAddr Addr | 
    MInv Inv |
    MGetData GetData |
    MNotFound NotFound |
    MGetBlocks GetBlocks |
    MGetHeaders GetHeaders |
    MTx Tx |
    MBlock Block |
    MHeaders Headers |
    MGetAddr |
    MPing Ping |
    MPong Pong |
    MAlert Alert
    deriving (Eq, Show)

instance Binary Message where

    get = do
        (MessageHeader mgc cmd len chk) <- get
        bs <- lookAhead $ getByteString $ fromIntegral len
        unless (mgc == networkMagic)
            (fail $ "get: Invalid network magic bytes: " ++ (show mgc))
        unless (chksum32 bs == chk) 
            (fail $ "get: Invalid message checksum: " ++ (show chk))
        if (fromIntegral len) > 0 
            then isolate (fromIntegral len) $ case cmd of
                MCVersion    -> MVersion <$> get
                MCAddr       -> MAddr <$> get
                MCInv        -> MInv <$> get
                MCGetData    -> MGetData <$> get
                MCNotFound   -> MNotFound <$> get
                MCGetBlocks  -> MGetBlocks <$> get
                MCGetHeaders -> MGetHeaders <$> get
                MCTx         -> MTx <$> get
                MCBlock      -> MBlock <$> get
                MCHeaders    -> MHeaders <$> get
                MCPing       -> MPing <$> get
                MCPong       -> MPong <$> get
                MCAlert      -> MAlert <$> get
                _            -> fail $ "get: Invalid command " ++ (show cmd)
            else case cmd of
                MCGetAddr    -> return MGetAddr 
                MCVerAck     -> return MVerAck
                _            -> fail $ "get: Invalid command " ++ (show cmd)

    put msg = do
        let (cmd, payload) = case msg of
                (MVersion m)    -> (MCVersion, encode' m)
                (MVerAck)       -> (MCVerAck, BS.empty)
                (MAddr m)       -> (MCAddr, encode' m)
                (MInv m)        -> (MCInv, encode' m)
                (MGetData m)    -> (MCGetData, encode' m)
                (MNotFound m)   -> (MCNotFound, encode' m)
                (MGetBlocks m)  -> (MCGetBlocks, encode' m)
                (MGetHeaders m) -> (MCGetHeaders, encode' m)
                (MTx m)         -> (MCTx, encode' m)
                (MBlock m)      -> (MCBlock, encode' m)
                (MHeaders m)    -> (MCHeaders, encode' m)
                (MGetAddr)      -> (MCGetAddr, BS.empty)
                (MPing m)       -> (MCPing, encode' m)
                (MPong m)       -> (MCPong, encode' m)
                (MAlert m)      -> (MCAlert, encode' m)
            chk = chksum32 payload
            len = fromIntegral $ BS.length payload
            head = MessageHeader networkMagic cmd len chk
        putByteString $ (encode' head) `BS.append` payload
        
