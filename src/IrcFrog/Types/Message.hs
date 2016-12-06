module IrcFrog.Types.Message where

import Data.DateTime (DateTime)
import Data.Text (Text)
import Data.Sequence (Seq)

import qualified Network.IRC.Conduit as IRC
import IrcFrog.Types.Connection

data ChannelName =
    ChannelName !Text
    deriving (Show)

data IrcUser =
    IrcUser !Text

data ChannelMessageType
    = UserMessage
    | JoinMessage
    | PartMessage
    | KickMessage
    | TopicMessage
    | InviteMessage

data ChannelMessage = ChannelMessage
    { cmContent :: IRC.IrcMessage
    , cmDate :: DateTime
    } deriving (Show)

data Channel = Channel
    { cName :: ChannelName
    , cMessages :: Seq ChannelMessage
    } deriving (Show)
