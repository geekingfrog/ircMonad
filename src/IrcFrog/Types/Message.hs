-- Different messages types than Network.IRC.Conduit.Message
-- to better fit this application

module IrcFrog.Types.Message where

import Data.Text (Text)
-- import Data.Text.Encoding (decodeUtf8')

data ChannelName = ChannelName !Text
data MessageContent = MessageContent !Text
data IrcUser = IrcUser !Text

data ChannelMessageType =
      UserMessage
    | JoinMessage
    | PartMessage
    | KickMessage
    | TopicMessage
    | InviteMessage

data ChannelMessage = ChannelMessage
    { channelName :: ChannelName
    , channelMessageType :: !ChannelMessageType
    , messageContent :: MessageContent
    }

data DisplayableMessage =
    ServerMessage !Int [Text]  -- Numerical response, decoded arguments
  -- | ChannelMessage ChannelName MessageContent
  | PrivMessage IrcUser MessageContent
