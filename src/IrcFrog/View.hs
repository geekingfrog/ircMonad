module IrcFrog.View where

import Data.Text (Text)
import qualified Data.Text as Text

import Lens.Micro
import qualified Graphics.Vty as V
import qualified Brick.Types as Brick
import qualified Brick.AttrMap as BrickAttr

import Brick.Widgets.Core ((<+>), (<=>))
import qualified Brick.Widgets.Core as Widget
import qualified Brick.Widgets.Center as Widget
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Border.Style as Border

import qualified IrcFrog.Types as Types

defaultTheme :: Types.Theme
defaultTheme = BrickAttr.attrMap V.defAttr []

render :: Types.AppState -> [Brick.Widget Text]
render appState =
    let
        nList = fmap (Widget.str . Text.unpack . Types.host) (appState ^. Types.networksL)
        chanContent = Widget.center $ Widget.str "chan content here"
    in
        [ networkList nList
        <+> (chanContent <=> editor)
        <+> userList
        ]

networkList networks =
      Widget.withBorderStyle Border.unicode
    $ Border.border
    $ Widget.hLimit 30
    $ Widget.center
    $ Widget.vBox networks

userList = -- Widget.withBorderStyle Border.unicode
      Widget.withBorderStyle Border.unicode
    $ Border.border
    $ Widget.hLimit 30
    $ Widget.center
    $ Widget.str "user list"

editor =
      Widget.withBorderStyle Border.unicode
    $ Border.border
    $ Widget.hCenter
    $ Widget.str "editor"
