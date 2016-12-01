module IrcFrog.Types.Network where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import qualified Control.Concurrent.STM.TBMChan as STM

import qualified Network.IRC.Conduit as IRC


data IrcUser = IrcUser !Text deriving (Show)
data IrcHostname = IrcHostname !ByteString deriving (Show)

-- immutable data about a network.
data NetworkEnv = NetworkEnv
    { _sendingQueue :: STM.TBMChan IRC.IrcMessage
    -- ^ Queue to send message to this network
    , _receivingQueue :: STM.TBMChan (Either ByteString IRC.IrcEvent)
    -- ^ All messages from the network are written in this queue
    , _hostname :: IrcHostname
    , _port :: !Int
    }

data NetworkState = NetworkState
    { _nick :: !IrcUser
    }

-- The monad in which a network connection is run
type StatefulNetwork = ReaderT NetworkEnv (StateT NetworkState IO) ()