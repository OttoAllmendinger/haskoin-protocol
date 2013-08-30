# haskoin-protocol

Implementation of the Bitcoin network protocol messages

Project Status: **Experimental**

## Description

**haskoin-protocol** is a component of **haskoin**, an ecosystem of haskell
libraries implementing the various parts of the bitcoin protocol. This library
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

Todo ...

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

For running the test suites

```sh
    cabal configure --enable-test
    cabal build
    cabal test
```

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

