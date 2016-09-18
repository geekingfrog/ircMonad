module IrcFrog.View where

import Data.Text (Text)

import qualified Graphics.Vty as V
import qualified Brick.Types as Brick
import qualified Brick.AttrMap as BrickAttr

import qualified IrcFrog.Types as Types

defaultTheme :: Types.Theme
defaultTheme = BrickAttr.attrMap V.defAttr []

render :: Types.AppState -> [Brick.Widget Text]
render = error "render not implemented yet"
