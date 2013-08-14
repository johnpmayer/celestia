
module Build where

import Char (toCode)
import Keyboard (KeyCode)

import open Draw
import open Types

updateMode : KeyCode -> BuildMode -> BuildMode
updateMode key mode =
  if | key == toCode '1' -> Inactive
     | key == toCode '2' -> BeamMode { r = 10 }
     | key == toCode '3' -> PartMode <| Brain { r = 5 }
     | key == toCode '4' -> PartMode <| FuelTank { l = 5, w = 5 }
     | key == toCode '5' -> PartMode <| Engine { r = 5, config = Disabled }
     | otherwise         -> mode

blueprint : BuildMode -> Form
blueprint mode = 
  case mode of 
    Inactive -> group []
    BeamMode beam -> drawBeam beam []
    PartMode part -> drawPart 0 [] part
