module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import qualified Haskoin.Protocol.Tests (tests)

main = defaultMain (Haskoin.Protocol.Tests.tests)

