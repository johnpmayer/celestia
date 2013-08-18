
module Utils where

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


