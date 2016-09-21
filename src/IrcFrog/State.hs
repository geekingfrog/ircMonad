module IrcFrog.State where

import Data.Text (Text)
import Safe (headMay)

import qualified Graphics.Vty as Vty
import qualified Brick.Types as Brick

import qualified IrcFrog.Types as Types

handleEvent
    :: Types.AppState
    -> Vty.Event
    -> Brick.EventM Text (Brick.Next Types.AppState)
handleEvent = error "handleEvent not yet implemented"

liftVtyEvent :: Vty.Event -> Types.AppEvent
liftVtyEvent = Types.BrickEvent

appChooseCursor
    :: Types.AppState
    -> [Brick.CursorLocation Text]
    -> Maybe (Brick.CursorLocation Text)
appChooseCursor _ = headMay

initialState :: Types.AppState
initialState =
    Types.AppState
    { Types.networks = []
    , Types.selectedNetwork = Nothing
    , Types.selectedChannel = Nothing
    }
