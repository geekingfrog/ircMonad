module IrcFrog.State where

import qualified IrcFrog.Types.Client as Types
import qualified Data.Map.Strict as Map

initialState :: Types.ClientState
initialState =
    Types.ClientState
    { Types.csNetworkList = Map.empty
    }
