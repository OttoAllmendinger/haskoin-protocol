module Haskoin.Protocol.Message ( Message(..) ) where

import Control.Monad (unless)
import Control.Applicative ((<$>))

import Data.Word (Word32)
import Data.Binary (Binary, get, put)
import Data.Binary.Get 
    ( lookAhead
    , getByteString
    )
import Data.Binary.Put 
    ( runPut
    , putByteString
    )
import qualified Data.ByteString as BS (length , append)

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

import Haskoin.Util (isolate, toStrictBS)
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
        isolate (fromIntegral len) $ case cmd of
            MCVersion    -> MVersion <$> get
            MCVerAck     -> return MVerAck
            MCAddr       -> MAddr <$> get
            MCInv        -> MInv <$> get
            MCGetData    -> MGetData <$> get
            MCNotFound   -> MNotFound <$> get
            MCGetBlocks  -> MGetBlocks <$> get
            MCGetHeaders -> MGetHeaders <$> get
            MCTx         -> MTx <$> get
            MCBlock      -> MBlock <$> get
            MCHeaders    -> MHeaders <$> get
            MCGetAddr    -> return MGetAddr 
            MCPing       -> MPing <$> get
            MCPong       -> MPong <$> get
            MCAlert      -> MAlert <$> get

    put msg = do
        let (cmd, mPut) = case msg of
                (MVersion m)    -> (MCVersion, put m)
                (MVerAck)       -> (MCVersion, return ())
                (MAddr m)       -> (MCAddr, put m)
                (MInv m)        -> (MCInv, put m)
                (MGetData m)    -> (MCGetData, put m)
                (MNotFound m)   -> (MCNotFound, put m)
                (MGetBlocks m)  -> (MCGetBlocks, put m)
                (MGetHeaders m) -> (MCGetHeaders, put m)
                (MTx m)         -> (MCTx, put m)
                (MBlock m)      -> (MCBlock, put m)
                (MHeaders m)    -> (MCHeaders, put m)
                (MGetAddr)      -> (MCGetAddr, return ())
                (MPing m)       -> (MCPing, put m)
                (MPong m)       -> (MCPong, put m)
                (MAlert m)      -> (MCAlert, put m)
            payload  = toStrictBS $ runPut mPut
            chk = chksum32 payload
            len = fromIntegral $ BS.length payload
            head = MessageHeader networkMagic cmd len chk
            headBS = toStrictBS $ runPut $ put head
        putByteString $ headBS `BS.append` payload
        
