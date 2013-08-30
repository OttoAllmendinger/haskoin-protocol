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

        -- Create a Version message (contains message header when serialized)
        versionMessage   = MVersion version

        -- Create a VerAck message
        verackMessage    = MVerAck

        -- Serialize the version and verack messages (contains message header)
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

