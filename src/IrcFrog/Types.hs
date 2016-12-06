{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module IrcFrog.Types where

import Data.Monoid ((<>))
import Network.IRC.Conduit.Internal.Messages
       (IrcMessage, ChannelName)
import qualified Network.IRC.Client.Types as IRC
import qualified Control.Concurrent.STM.TBMChan as STM
import qualified Brick.Widgets.Edit as Brick
import qualified Brick.Main as Brick
import qualified Brick.AttrMap as BrickAttr
import qualified Graphics.Vty.Input.Events as VtyEvents

import IrcFrog.Types.TH (suffixLenses)

import Data.Text as T

data Channel = Channel
    { name :: ChannelName Text
    , editor :: Brick.Editor Text Text
    , content :: [Text]
      -- TODO Use something else here, a list is just asking for memory leak.
      -- but as a first approximation it's perfectly fine
    }

suffixLenses ''Channel

instance Show Channel where
    show chan = T.unpack (name chan)

data Network = Network
    { host :: Text
    , port :: Int
    , nick :: Text
      -- ^ nickname used for this channel
    , outboundChan :: STM.TBMChan IrcMessage
      -- ^ queue to send message to this channel
    , inboundChan :: STM.TBMChan IRC.UnicodeEvent
      -- ^ queue to receive the messages
    , channels :: [ChannelName Text]
    }

suffixLenses ''Network

instance Show Network where
    show n =
        T.unpack $
        "Network " <> host n <> ":" <> T.pack (show $ port n) <> " (" <> nick n <>
        "). Connected to " <>
        T.pack (show (T.unpack <$> channels n))

instance Eq Network where
    n1 == n2 = host n1 == host n2

data AppState = AppState
    { networks :: [Network]
    , selectedNetwork :: Maybe Network
    , selectedChannel :: Maybe (ChannelName Text)
    } deriving (Show)

suffixLenses ''AppState

type Theme = BrickAttr.AttrMap

type IrcApp = Brick.App AppState AppEvent Text

data AppEvent =
    BrickEvent VtyEvents.Event
    deriving (Show, Eq)
