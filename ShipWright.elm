
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


module ShipWright where

import Mouse as M

import open Draw
import open TagTree
import open Types
import open Utils
import open Vec2

dimensions = (800,600)

gamePointer = (convertPos dimensions) <~ M.position

initialShip = beam {r=100} <|
  [ ({offset=70,theta=(pi/2)}, beam {r=30} [])
  , ({offset=40,theta=(-pi/2)}, beam {r=60} [])
  ]

stopped = { v = origin, pos = {x=0,y=0,theta=0}, omega = 0} 

ship = foldp (flip cnst) initialShip (constant ())

shipWith = (placeStructure (Brain {r=10})) <~ gamePointer ~ ship

display = drawStructure 0 []

space = (\sfs -> spaceBlack dimensions <~ combine sfs)
  [ display <~ shipWith ]

main = combineSElems <|
  [ space
  , (asText . buildPoints . labelBeams) <~ ship
  , asText <~ (bestPlacement <~ gamePointer ~ ship)
  ]

{- Hard Stuff -}

type BuildPoint = {id : Int, start : Vec2, end : Vec2}

buildPoints : LabelStructure -> [BuildPoint]
buildPoints = 
  let partPoints = cnst []
      beamPoint beam = 
        { id = beam.id, start = origin, end = {x=beam.r,y=0}}
      beamPoints beam subs = beamPoint beam :: concat subs
      attachPoint {offset,theta} bp = 
        let place = addVec {x=offset,y=0} . rotVec theta
        in { bp | start <- place bp.start, end <- place bp.end }
      attachPoints attach = map (attachPoint attach)
  in foldTagTree partPoints beamPoints attachPoints

type LabelDist = { id:Int, r:Float, offset:Float }

pointDist : (Int,Int) -> BuildPoint -> LabelDist
pointDist (x,y) bp = 
  let xF = toFloat x
      yF = toFloat y
      dist = minimumDist bp.start bp.end {x=xF,y=yF}
  in { dist | id = bp.id }

compareDists : LabelDist -> LabelDist -> LabelDist
compareDists p1 p2 = if p1.r < p2.r then p1 else p2

minPointDist : (Int,Int) -> [BuildPoint] -> LabelDist
minPointDist pointer bps = 
  let minDist = 50
      fakePoint = { id=-1, r=minDist, offset=0 }
      labeledDists = map (pointDist pointer) bps
  in foldl compareDists fakePoint labeledDists

bestPlacement : (Int,Int) -> Structure -> LabelDist
bestPlacement pointer = 
  minPointDist pointer . buildPoints . labelBeams

placeStructure : Part -> (Int,Int) -> Structure -> Structure
placeStructure p pointer s =
  let best = bestPlacement pointer s
  in  if best.id < 0
      then s
      else
        let placePart p = part p
            placeBeam b subs = beam {b - id} <|
              if best.id == b.id
              then ({offset=best.offset,theta=0}, part p) :: subs
              else subs
        in foldTagTree' placePart placeBeam <| labelBeams s
