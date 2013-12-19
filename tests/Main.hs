module Main where

import Test.Framework (defaultMain)

import qualified Network.Haskoin.Protocol.Tests (tests)
import qualified Units (tests)

main :: IO ()
main = defaultMain 
    (  Network.Haskoin.Protocol.Tests.tests
    ++ Units.tests
    )

