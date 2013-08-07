module Types where

type Attach =
  { offset : Float
  , theta : Float 
  }

type Vec2 =
  { x : Float
  , y : Float
  }

type Position = 
  { x : Float
  , y : Float
  , theta : Float 
  }

type PointMass = 
  { x : Float
  , y : Float
  , m : Float
  }

type Beam = 
  { l : Float 
  }

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

type Structure = TagTree Part Beam Attach

data EngineConfig = Thrust 
                  | TurnLeft 
                  | TurnRight

data Part = Brain { r : Float }
          | FuelTank { l : Float, w : Float }
          | Engine { r : Float, config : EngineConfig }

