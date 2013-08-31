import Data.Binary (encode, decodeOrFail)
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

        -- Create a Version message 
        -- Will contain a message header when serialized
        versionMessage   = MVersion version

        -- Create a VerAck message
        -- Will contain a message header when serialized
        verackMessage    = MVerAck

        -- Serialize the version and verack messages 
        -- This produces a Data.ByteString.Lazy
        -- Check the Data.Binary package for more details
        versionMessageBS = encode versionMessage
        verackMessageBS  = encode verackMessage

    print $ "Serialized version message: " ++ (show versionMessageBS)
    print $ "Serialized verAck message: "  ++ (show verackMessageBS)

        -- Deserialize the version and verack messages from bytestrings
        -- Check the Data.Binary package for more details
    let originalVersion = case decodeOrFail versionMessageBS of
            (Left  (_, _, err)) -> error err
            (Right (_, _, msg)) -> msg :: Message

        originalVerack = case decodeOrFail verackMessageBS of
            (Left  (_, _, err)) -> error err
            (Right (_, _, msg)) -> msg :: Message

    print $ "De-serialized version message: " ++ (show originalVersion)
    print $ "De-serialized verack message: "  ++ (show originalVerack)

