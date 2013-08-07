module Types where

{- 2D Vector -}

type Vec2 =
  { x : Float
  , y : Float
  }

origin : Vec2
origin = { x = 0, y = 0 }

scaleVec : 
  Float ->
  { b | x : Float, y : Float } ->
  { b | x : Float, y : Float }
scaleVec a v = { v | x <- v.x * a, y <- v.y * a }

addVec :
  { a | x : Float, y : Float } ->
  { b | x : Float, y : Float } ->
  { b | x : Float, y : Float }
addVec v1 v2 = 
  let newX = v1.x + v2.x
      newY = v1.y + v2.y
  in { v2 | x <- newX, y <- newY }

{- TagTree -}

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

{- Structures -}

type Attach =
  { offset : Float
  , theta : Float 
  }

translateAttach :
  Attach ->
  { a | x : Float, y : Float } ->
  { a | x : Float, y : Float }
translateAttach attach = 
  let dX = attach.offset * cos attach.theta
      dY = attach.offset * sin attach.theta
  in addVec { x = dX, y = dY }

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

type Structure = TagTree Part Beam Attach

data EngineConfig = Thrust 
                  | TurnLeft 
                  | TurnRight

data Part = Brain { r : Float }
          | FuelTank { l : Float, w : Float }
          | Engine { r : Float, config : EngineConfig }

