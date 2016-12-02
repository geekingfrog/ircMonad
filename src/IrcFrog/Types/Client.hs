{-# LANGUAGE TemplateHaskell #-}

module IrcFrog.Types.Client where

import Data.Text (Text)
import qualified Data.Map.Strict as Map

import qualified Brick.Main as Brick
import qualified Graphics.Vty.Input.Events as VtyEvents

import qualified IrcFrog.Types.Connection as Connection
import qualified Network.IRC.Conduit as IRC

import IrcFrog.Types.TH (suffixLenses)

data ClientNetwork = ClientNetwork
    { connectionInfo :: Connection.ConnectionEnv
    , nick :: IRC.NickName Text
    , connectionState :: Connection.NetworkConnectionState
    }

suffixLenses ''ClientNetwork

data NetworkList = NetworkList
    { networks :: Map.Map Connection.IrcHostname ClientNetwork
    }

suffixLenses ''NetworkList

data ClientState = ClientState
    { networkList :: !NetworkList
    }

suffixLenses ''ClientState


data AppEvent =
      VtyEvent VtyEvents.Event
    | ConnectionEvent Connection.NetworkEvent
    deriving (Show, Eq)

type IrcApp = Brick.App ClientState AppEvent Text
