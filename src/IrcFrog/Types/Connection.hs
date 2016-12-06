{-# LANGUAGE TemplateHaskell #-}

module IrcFrog.Types.Connection where

import Data.ByteString (ByteString)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TBMChan as STM

import qualified Network.IRC.Conduit as IRC
import IrcFrog.Types.Network

import IrcFrog.Types.TH (suffixLenses)

-- immutable data about a connection.
data ConnectionEnv = ConnectionEnv
    { ceSendingQueue :: STM.TBMChan IRC.IrcMessage
      -- ^ Queue to send message to this network
    , ceReceivingQueue :: STM.TBMChan NetworkEvent
      -- ^ All messages from the network are written in this queue
    , ceNetworkId :: !NetworkId
    }

suffixLenses ''ConnectionEnv

data ConnectionState = ConnectionState
    { connectionState :: STM.TVar NetworkConnectionState
    }

-- The monad in which a network connection is run
type StatefulNetwork = ReaderT ConnectionEnv (StateT ConnectionState IO) ()
