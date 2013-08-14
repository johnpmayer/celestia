module Types where

import open Vec2

{- Utils -}

cnst : a -> b -> a
cnst x = \a -> x

data TagTree leaf node edge 
  = Leaf leaf
  | Node node [(edge, TagTree leaf node edge)]

foldTagTree :
  (leaf -> a) ->
  (node -> [a] -> a) ->
  (edge -> a -> a) ->
  TagTree leaf node edge -> a
foldTagTree fLeaf fNode fEdge tree =
  let y = foldTagTree fLeaf fNode fEdge
      fChild (edge, child) = fEdge edge <| y child
  in case tree of
    Leaf leaf -> fLeaf leaf
    Node node subs -> fNode node <| map fChild subs

type Position = { x : Float, y : Float, theta : Float }

{- Structures -}

type Attach ={ offset : Float, theta : Float }

translateAttach : Attach -> Vec2Ext a -> Vec2Ext a
translateAttach attach vext = 
  let dX = attach.offset * cos attach.theta
      dY = attach.offset * sin attach.theta
  in addVec { x = dX, y = dY } vext

type PointMass = { x : Float, y : Float, m : Float}

type Beam = { r : Float }

type Structure = TagTree Part Beam Attach

data EngineConfig = Disabled
                  | Forward 
                  | Reverse
                  | TurnLeft 
                  | TurnRight

data Part = Brain { r : Float }
          | FuelTank { l : Float, w : Float }
          | Engine { r : Float, config : EngineConfig }

{- Physics -}

type MotionState = { pos : Position, v : Vec2, omega : Float }

type MotionDelta = { a : Vec2, alpha : Float }

type Thrust = { disp : Vec2, force : Vec2 }

{- Build -}

data BuildMode = Inactive
               | BeamMode Beam
               | PartMode Part
