module IrcFrog.Client where

import IrcFrog.Types.Client as Type
import Data.Default (def)
import Data.Text (Text)

import qualified Control.Concurrent.Chan as Chan
import qualified Brick.Main as Brick
import qualified Brick.Types as BT
import Brick.AttrMap (attrMap)
import qualified Graphics.Vty as V

import qualified IrcFrog.State as State
import IrcFrog.Views (render)

run :: Chan.Chan Type.AppEvent -> IO Type.ClientState
run chan = Brick.customMain (V.mkVty def) (Just chan) app State.initialState

app :: Type.IrcApp
app = Brick.App
    { Brick.appDraw = render
    , Brick.appChooseCursor = \_s _locations -> Nothing
    , Brick.appHandleEvent = tmpEventHandler
    , Brick.appStartEvent = return
    , Brick.appAttrMap = const $ attrMap def []
    }


tmpEventHandler
    :: Type.ClientState
    -> BT.BrickEvent Text Type.AppEvent
    -> BT.EventM Text (BT.Next Type.ClientState)
tmpEventHandler state _ = Brick.halt state
-- tmpEventHandler state _ = Brick.continue state
