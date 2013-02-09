
module Draw where

import Types

drawSpaceCraft : SpaceCraft -> Form
drawSpaceCraft _ = filled red $ rect 10 10 (0,0)
