module IrcFrog.State where

import qualified IrcFrog.Types.Client as Types
import qualified Data.Map.Strict as Map

initialState :: Types.ClientState
initialState =
    let list =
            Types.NetworkList
            { Types.networks = Map.empty
            }
    in Types.ClientState
       { Types.networkList = list
       }
