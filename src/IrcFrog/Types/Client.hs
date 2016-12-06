{-# LANGUAGE TemplateHaskell #-}

module IrcFrog.Types.Client where

import Data.Text (Text)
import qualified Data.Map.Strict as Map

import qualified Brick.Main as Brick
import qualified Graphics.Vty.Input.Events as VtyEvents

import IrcFrog.Types.Connection
import IrcFrog.Types.Network
import IrcFrog.Types.Message

import qualified Network.IRC.Conduit as IRC

import IrcFrog.Types.TH (suffixLenses)

data ClientNetwork = ClientNetwork
    { cnConnectionEnv :: ConnectionEnv
    , cnNick :: IRC.NickName Text
    , cnConnectionState :: NetworkConnectionState
    , cnChannels :: Map.Map ChannelName Channel
    }

suffixLenses ''ClientNetwork

data ClientState = ClientState
    { csNetworkList :: Map.Map NetworkId ClientNetwork
    }

suffixLenses ''ClientState

data AppEvent
    = VtyEvent VtyEvents.Event
    | ConnectionEvent !NetworkId
                      !NetworkEvent
    deriving (Show, Eq)

type IrcApp = Brick.App ClientState AppEvent Text
