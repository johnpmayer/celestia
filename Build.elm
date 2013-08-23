
{-
    Copyright (c) John P Mayer, Jr 2013

    This file is part of celestia.

    celestia is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    celestia is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with celestia.  If not, see <http://www.gnu.org/licenses/>.
-}


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
