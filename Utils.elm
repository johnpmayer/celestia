
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

import Color exposing (..)
import Data.Vec2 exposing (..)
import Dict exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Signal exposing (..)
import Signal.Extra exposing (combine)

import Types exposing (..)

cnst : a -> b -> a
cnst x = \a -> x

convertPos : (Int,Int) -> (Int,Int) -> (Int,Int)
convertPos (w,h) (x,y) = (x - w // 2, h // 2 - y)

combineSElems : Direction -> List (Signal Element) -> Signal Element
combineSElems dir ses = flow dir <~ combine ses

spaceBlack : (Int,Int) -> List Form -> Element
spaceBlack (w,h) stuff = collage w h <|
  (filled black <| rect (toFloat w) (toFloat h)) ::
  stuff

controlEngines : { x:Int, y:Int } -> List EngineConfig
controlEngines input = 
  (if input.y < 0 then [ Reverse ] else []) ++
  (if input.y > 0 then [ Forward ] else []) ++
  (if input.x > 0 then [ TurnRight ] else []) ++
  (if input.x < 0 then [ TurnLeft ] else [])

updateDict : comparable -> (v -> v) -> Dict comparable v -> Dict comparable v
updateDict k f d =
  case get k d of
    Nothing -> d
    Just v -> insert k (f v) d

genGameStateCache : Int -> Dict Int Entity -> GameStateCache
genGameStateCache focus entities = 
  let camera = case get focus entities of
    Nothing -> origin
    Just e ->
      let pos = e.motion.pos
          rootPos = extractVec pos
          comOffset = rotVec pos.theta e.cache.comOffset
      in addVec comOffset rootPos
  in GameStateCache camera

prepend : String -> String -> String
prepend p s = p ++ s

rndModulo : Float -> Float -> Float
rndModulo f n = f * (toFloat (round (n / f)))

justs : List (Maybe a) -> List a
justs = List.foldl (\mx xs -> case mx of
  Nothing -> xs
  Just x -> x :: xs) []
