module IrcFrog.Client where

import IrcFrog.Types.Client as Type
import Data.Default (def)

import qualified Control.Concurrent.Chan as Chan
import qualified Brick.Main as Brick
import qualified Graphics.Vty as V

import qualified IrcFrog.State as State

run :: Chan.Chan Type.AppEvent -> IO (Type.ClientState)
run chan =
    let
        initialState = undefined
    in
        Brick.customMain (V.mkVty def) (Just chan) app State.initialState

app :: Type.IrcApp
app = Brick.App
    { Brick.appDraw = undefined
    , Brick.appChooseCursor = \_s _locations -> Nothing
    , Brick.appHandleEvent = undefined
    , Brick.appStartEvent = undefined
    , Brick.appAttrMap = undefined
    }
