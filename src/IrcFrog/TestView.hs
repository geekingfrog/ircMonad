{-# LANGUAGE OverloadedStrings #-}

module IrcFrog.TestView where

import Data.Text as T

import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Types as Brick
import qualified Brick.AttrMap as A
import Control.Concurrent (newChan)
import qualified Control.Concurrent.STM.TBMChan as STM
import Network.IRC.Conduit.Internal.Messages (IrcMessage)
import qualified Data.Default as Default

import qualified Brick.Widgets.Core as W
import qualified Brick.Widgets.Center as W
import qualified Brick.Widgets.Edit as WE

type AppState = WE.Editor Text Text
data AppEvent = Ev deriving (Show, Eq)

testView :: IO ()
testView = do
    let app = M.App { M.appDraw = drawUI
                    , M.appStartEvent = return
                    , M.appHandleEvent = handleEvent
                    , M.appAttrMap = const (A.attrMap V.defAttr [])
                    , M.appLiftVtyEvent = id
                    , M.appChooseCursor = \_ locs -> (Just $ Prelude.head locs)
                    }
    chan <- newChan
    let ed = WE.editorText "editorName" (W.txt . T.unlines) (Just 1) "Initial content"
    _ <- M.customMain (V.mkVty Default.def) chan app ed
    putStrLn "done"

drawUI :: AppState -> [Brick.Widget Text]
drawUI st = [ W.vBox
    [ W.center $ W.str "foo"
    , WE.renderEditor True st
    ] ]

handleEvent :: AppState -> V.Event -> Brick.EventM Text (Brick.Next AppState)
handleEvent st ev = case ev of
    V.EvKey V.KEsc [] -> M.halt st
    _ -> WE.handleEditorEvent ev st >>= M.continue
