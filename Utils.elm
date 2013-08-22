
module Utils where

import open Types

cnst : a -> b -> a
cnst x = \a -> x

convertPos : (Int,Int) -> (Int,Int) -> (Int,Int)
convertPos (w,h) (x,y) = (x - div w 2, div h 2 - y)

combineSElems : [Signal Element] -> Signal Element
combineSElems ses = flow down <~ combine ses

spaceBlack : (Int,Int) -> [Form] -> Element
spaceBlack (w,h) stuff = collage w h <|
  (filled black <| rect (toFloat w) (toFloat h)) ::
  stuff

controlEngines : { x:Int, y:Int } -> [ EngineConfig ]
controlEngines input = 
  (if input.y < 0 then [ Reverse ] else []) ++
  (if input.y > 0 then [ Forward ] else []) ++
  (if input.x > 0 then [ TurnRight ] else []) ++
  (if input.x < 0 then [ TurnLeft ] else [])


