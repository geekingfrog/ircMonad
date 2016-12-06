module IrcFrog.Views.TextWidget where

import Data.Foldable (foldl')
import Data.Text (Text)

import Brick.Types (Widget)
import Brick.Widgets.Core as Widgets

renderTextWidget :: [Text] -> Widget Text
renderTextWidget messages =
    let lines = fmap Widgets.txt messages
    in foldl' (<=>) Widgets.emptyWidget lines
