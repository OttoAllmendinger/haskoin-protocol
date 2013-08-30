# haskoin-protocol

Implementation of the Bitcoin network protocol messages

Project Status: **Experimental**

## Description

**haskoin-protocol** is a component of **haskoin**, an ecosystem of haskell
libraries implementing the various parts of the Bitcoin protocol. This library
provides an implementation of the network types and messages used in the
Bitcoin protocol. It also provides the binary serialization and
de-serialization routines required for sending and reading the Bitcoin protocol
messages from the network.

## Synopsis

```haskell

    import Data.Binary (get, put)
    import Data.Binary.Get (runGet)
    import Data.Binary.Put (runPut)

    import Haskoin.Protocol
    import Haskoin.Util (stringToBS)

    main :: IO ()
    main = do
            -- Create a NetworkAddress type
        let networkAddress = NetworkAddress 
                                { naServices = 1
                                , naAddress  = (0xffff00, 0x000000)
                                , naPort     = 0
                                }
            -- Create a VarString type
            userAgent      = VarString $ stringToBS "/haskoin:0.0.1/"

            -- Create a Version type
            version        = Version 
                                { version     = 70000
                                , services    = 1
                                , timestamp   = 0
                                , addrRecv    = networkAddress
                                , addrSend    = networkAddress
                                , verNonce    = 0
                                , userAgent   = userAgent
                                , startHeight = 0
                                , relay       = True
                                }

            -- Create version and verack messages 
            -- Contains message headers when serialized
            versionMessage   = MVersion version
            verackMessage    = MVerAck

            -- Serialize version and verack messages (contains message header)
            -- This produces a Data.ByteString.Lazy 
            -- Check the Data.Binary.Put package for more details
            versionMessageBS = runPut $ put versionMessage
            verackMessageBS  = runPut $ put verackMessage

        print $ "Serialized version message: " ++ (show versionMessageBS)
        print $ "Serialized verAck message: "  ++ (show verackMessageBS)

            -- Deserialize the version and verack messages from bytestrings
            -- Check the Data.Binary.Get package for more details
        let originalVersion = runGet get versionMessageBS :: Message
            originalVerack  = runGet get verackMessageBS  :: Message

        print $ "De-serialized version message: " ++ (show originalVersion)
        print $ "De-serialized verack message: "  ++ (show originalVerack)

```

## Usage

All the types and functions in this section are exported by `Haskoin.Protocol`

```haskell
  import Haskoin.Protocol
```

All types in this library are instances of `Data.Binary` and can be serialized
and de-serialized easily. Here is an example using the `VarInt` type:

```haskell
    import Haskoin.Protocol
    import Data.Binary (get, put)
    import Data.Binary.Get (runGet)
    import Data.Binary.Put (runPut)

    main :: IO ()
    main = do
        let varint   = VarInt 10 

            -- Serialize a bitcoin protocol type
            -- See Data.Binary.Put for more details
            bs       = runPut $ put varint

            -- De-serialize a bitcoin protocol type
            -- See Data.Binary.Get for more details
            original = runGet get bs :: VarInt

        return ()
```

## Bitcoin Protocol Messages

Bitcoin protocol messages are exchanged between Bitcoin nodes. The definition
of these messages in given here.

### MessageHeader

```haskell
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

    data MessageHeader = MessageHeader {
        headMagic       :: Word32,
        headCmd         :: MessageCommand,
        headPayloadSize :: Word32,
        headChecksum    :: CheckSum32
    }
```

### Message

```haskell
    -- Algebraic data type describing a Bitcoin protocol message
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
```

## Protocol Types

### VarInt

```haskell
    newtype VarInt = VarInt { getVarInt :: Word64 }
```

### VarString

```haskell
    newtype VarString = VarString { getVarString :: BS.ByteString }
```

### NetworkAddress

```haskell
    data NetworkAddress = NetworkAddress {
        naServices :: Word64,
        naAddress  :: (Word64, Word64),
        naPort     :: Word16
    }
```

### Script

```haskell
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
        OP_PUBKEY PublicKey |
        OP_INVALIDOPCODE Word8

    newtype Script = Script { runScript :: [ScriptOp] }
```

### Tx

```haskell
    data Tx = Tx {
        txVersion  :: Word32,
        txIn       :: [TxIn],
        txOut      :: [TxOut],
        txLockTime :: Word32
    }

    data CoinbaseTx = CoinbaseTx {
        cbVersion  :: Word32,
        cbData     :: BS.ByteString,
        cbOut      :: [TxOut],
        cbLockTime :: Word32
    } 

    data TxIn = TxIn {
        prevOutput   :: OutPoint,
        sigScript    :: Script,
        txInSequence :: Word32
    } 

    data TxOut = TxOut {
        outValue     :: Word64,
        scriptPubKey :: Script
    }

    data OutPoint = OutPoint {
        outPointHash  :: Hash256,
        outPointIndex :: Word32
    }

```

### BlockHeader

```haskell
    data BlockHeader = BlockHeader {
        blockVersion   :: Word32,
        prevBlock      :: Hash256,
        merkleRoot     :: Hash256,
        blockTimestamp :: Word32,
        blockBits      :: Word32,
        bhNonce        :: Word32
    } 
```

### Block

```haskell
    data Block = Block {
        blockHeader     :: BlockHeader,
        blockCoinbaseTx :: CoinbaseTx,
        blockTxns       :: [Tx]
    } 
```

### GetBlocks

```haskell
    type BlockLocator = [Hash256]

    data GetBlocks = GetBlocks {
        getBlocksVersion  :: Word32,
        getBlocksLocator  :: BlockLocator,
        getBlocksHashStop :: Hash256
    } 
```

### GetHeaders

```haskell
    data GetHeaders = GetHeaders {
        getHeadersVersion  :: Word32,
        getHeadersBL       :: BlockLocator,
        getHeadersHashStop :: Hash256
    } 
```

### InvVector

```haskell
    data InvType = InvError | InvTx | InvBlock

    data InvVector = InvVector {
        invType :: InvType,
        invHash :: Hash256
    }
```

### GetData

```haskell
    data GetData = GetData {
      getDataList :: [InvVector] 
    }
```

### Inv

```haskell
    data Inv = Inv {
      invList :: [InvVector] 
    }
```

### Headers

```haskell
    type BlockHeaderCount = (BlockHeader, VarInt)

    data Headers = Headers {
        headersList :: [BlockHeaderCount]
    } 
```

### Version

```haskell
    data Version = Version {
        version     :: Word32,
        services    :: Word64,
        timestamp   :: Word64,
        addrRecv    :: NetworkAddress,
        addrSend    :: NetworkAddress,
        verNonce    :: Word64,
        userAgent   :: VarString,
        startHeight :: Word32,
        relay       :: Bool
    }
```

### Addr

```haskell
    type NetworkAddressTime = (Word32, NetworkAddress)

    data Addr = Addr {
        addrList :: [NetworkAddressTime]
    } 
```

### Ping

```haskell
    newtype Ping = Ping { pingNonce :: Word64 } 

    newtype Pong = Pong { pongNonce :: Word64 } 
```

### NotFound

```haskell
    data NotFound = NotFound {
      notFoundList :: [InvVector] 
    }
```

### Alert

```haskell
    data Alert = Alert {
        alertPayload   :: VarString,
        alertSignature :: VarString
    } 
```

## Dependencies

- Cabal package manager

```sh
    # in Ubuntu
    apt-get install cabal-install
```

- haskoin-util

```sh
    # haskoin-util is not on Hackage (yet) 
    git clone https://github.com/plaprade/haskoin-util.git
    cd haskoin-util
    cabal install
```

- haskoin-crypto

```sh
    # haskoin-crypto is not on Hackage (yet) 
    git clone https://github.com/plaprade/haskoin-crypto.git
    cd haskoin-crypto
    cabal install
```

## Installing

```sh
    # haskoin-protocol is not on Hackage (yet) 
    git clone https://github.com/plaprade/haskoin-protocol.git
    cd haskoin-protocol
    cabal install
```

### Tests

If you are missing the test dependencies:

```sh
    cabal install --enable-tests
    cabal test
```

If you have the test dependencies, you can build without installing:

```sh
    cabal configure --enable-tests
    cabal build
    cabal test
```

The tests can take a few minutes to run.

## Bugs

Please report any bugs in the projects bug tracker:

[github.com/plaprade/haskoin-protocol/issues](http://github.com/plaprade/haskoin-protocol/issues)

## Contributing

We're glad you want to contribute! It's simple:

- Fork haskoin-protocol
- Create a branch `git checkout -b my_branch`
- Commit your changes `git commit -am 'comments'`
- Push the branch `git push origin my_branch`
- Open a pull request

Code guidelines:

- 80 columns.
- 4 space indentation. No tabs.
- Follow the general style of the code, whenever it makes sense.

## Supporting

You can support the project by donating in [Bitcoins](http://www.bitcoin.org)
to:

**176CwMCWMq1y9CxFZWk7Vfoka5PoaNzxRq**

