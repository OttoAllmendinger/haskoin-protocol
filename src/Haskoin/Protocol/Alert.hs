module Haskoin.Protocol.Alert ( Alert(..) ) where

import Control.Applicative ((<$>),(<*>))
import Data.Binary (Binary, get, put)
import Haskoin.Protocol.VarString

import Data.Binary (Binary, get, put)

data Alert = Alert {
    alertPayload   :: !VarString,
    alertSignature :: !VarString
} deriving (Eq, Show)

instance Binary Alert where
    get = Alert <$> get <*> get
    put (Alert p s) = put p >> put s

