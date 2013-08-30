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

    -- Todo ...

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

