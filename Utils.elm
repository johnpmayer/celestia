
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


