
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


module Step where

import Dict (Dict)

import open Types

import Public.State.State as ST

import open Utils

type GameState = { entities : Dict Int Entity, mode : Mode }

type Mode = { pause : Bool, build : BuildMode }

data BuildMode = None | Engine | FuelTank

type GameStep = ST.State GameState ()

step : GameInput -> GameStep
step t = ST.returnS ()


