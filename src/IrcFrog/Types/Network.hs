module IrcFrog.Types.Network where

import Data.ByteString (ByteString)
import qualified Network.IRC.Conduit as IRC

data NetworkHostname =
    NetworkHostname !ByteString
    deriving (Show, Eq)

data NetworkPort =
    NetworkPort !Int
    deriving (Show, Eq)

-- Different kind of events coming from a connection.
data NetworkEvent
    = MsgEvent IRC.IrcEvent
    | ConnectionEvent NetworkConnectionState
    deriving (Show, Eq)

data NetworkConnectionState
    = Disconnected
    | Disconnecting
    | Connected
    | Connecting
    deriving (Show, Eq)

data NetworkId =
    NetworkId !NetworkHostname
              !NetworkPort
    deriving (Show, Eq)
