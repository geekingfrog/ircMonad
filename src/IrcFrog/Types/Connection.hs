{-# LANGUAGE TemplateHaskell #-}

module IrcFrog.Types.Connection where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TBMChan as STM

import qualified Network.IRC.Conduit as IRC

import IrcFrog.Types.TH (suffixLenses)

data IrcHostname = IrcHostname !ByteString deriving (Show, Eq)


-- Different kind of events coming from a connection.
data NetworkEvent =
      MsgEvent IRC.IrcEvent
    | ConnectionEvent NetworkConnectionState
    deriving (Show, Eq)


data NetworkConnectionState =
      Disconnected
    | Disconnecting
    | Connected
    | Connecting
    deriving (Show, Eq)


-- immutable data about a connection.
data ConnectionEnv = ConnectionEnv
    { sendingQueue :: STM.TBMChan IRC.IrcMessage
    -- ^ Queue to send message to this network
    , receivingQueue :: STM.TBMChan NetworkEvent
    -- ^ All messages from the network are written in this queue
    , hostname :: IrcHostname
    , port :: !Int
    }

suffixLenses ''ConnectionEnv

data ConnectionState = ConnectionState
    { connectionState :: STM.TVar NetworkConnectionState
    }

-- The monad in which a network connection is run
type StatefulNetwork = ReaderT ConnectionEnv (StateT ConnectionState IO) ()
