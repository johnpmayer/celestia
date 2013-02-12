module Main where

import Draw
import Types

-- These need to be copied into every file now (12 lines)
type Size = 
  { length : Float
  , ratio  : Float }
type Attach =
  { offset : Float
  , facing : Float }
type Ship = 
  { x      : Float 
  , y      : Float 
  , facing : Float
  , struct : Structure }

mods : [(Attach, Structure)]
mods = 
 [ ({ offset= 0, facing=0 }, Node { length = 10, ratio = 1 } Brain)
 , ({ offset=25, facing=0 }, Node { length = 20, ratio = 1.2 } FuelTank)
 , ({ offset=50, facing=0 }, Node { length = 10, ratio = 1 } Engine) ]

simpleShip : Ship
simpleShip =
  { x      = 50
  , y      = 50
  , facing = 0.25
  , struct = Node { length = 50, ratio = 0.05 } $ Beam mods }

main : Element
main = collage 400 300 $ drawShip simpleShip
