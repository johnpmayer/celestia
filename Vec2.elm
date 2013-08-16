module Vec2 where

import Matrix2D (Matrix2D, matrix)

{- 2D Vector -}

type Vec2Ext a = { a | x : Float, y : Float }

type Vec2 = Vec2Ext {}

origin : Vec2
origin = { x = 0, y = 0 }

fromIntPair : (Int,Int) -> Vec2
fromIntPair (x,y) = { x = toFloat x, y = toFloat y }

scaleVec : Float -> Vec2Ext a -> Vec2Ext a
scaleVec a v = { v | x <- v.x * a, y <- v.y * a }

negVec : Vec2Ext a -> Vec2Ext a
negVec = scaleVec -1

rotVec : Float -> Vec2Ext a -> Vec2Ext a
rotVec theta v =
  let newX = v.x * cos theta - v.y * sin theta
      newY = v.x * sin theta + v.y * cos theta
  in { v | x <- newX, y <- newY }

addVec : Vec2Ext a -> Vec2Ext b -> Vec2Ext b
addVec v1 v2 = 
  let newX = v1.x + v2.x
      newY = v1.y + v2.y
  in { v2 | x <- newX, y <- newY }

-- subtract the first vector from the second vector
subVec : Vec2Ext a -> Vec2Ext b -> Vec2Ext b
subVec v1 = addVec <| scaleVec (-1) v1

addVec2 : Vec2 -> Vec2 -> Vec2
addVec2 = addVec

midVec : Vec2Ext a -> Vec2Ext b -> Vec2Ext b
midVec v1 v2 = scaleVec (1/2) <| addVec v1 v2 

extractVec : Vec2Ext a -> Vec2
extractVec v = { x = v.x, y = v.y }

sumVec : [ Vec2Ext a ] -> Vec2
sumVec = foldl addVec origin . map extractVec

magnitude : Vec2Ext a -> Float
magnitude v = sqrt <| v.x * v.x + v.y * v.y

magSquared : Vec2Ext a -> Float
magSquared v = v.x * v.x + v.y * v.y

distance : Vec2Ext a -> Vec2Ext b -> Float
distance v1 v2 = magnitude <| subVec v2 v1

distanceSquared : Vec2Ext a -> Vec2Ext b -> Float
distanceSquared v1 v2 = magSquared <| subVec v2 v1

dotVec : Vec2Ext a -> Vec2Ext b -> Float
dotVec v1 v2 = v1.x * v2.x + v1.y * v2.y

crossVecMag : Vec2Ext a -> Vec2Ext b -> Float
crossVecMag v1 v2 =
  v1.x * v2.y - v1.y * v2.x

vecTranslate : Vec2Ext a -> Matrix2D
vecTranslate v = matrix 1 0 0 1 v.x v.y

-- find the minimum distance and offset of a line segment and a point
minimumDist : Vec2Ext a -> Vec2Ext a -> Vec2Ext b -> { r : Float, offset : Float }
minimumDist v w p = 
  let l2 = distanceSquared v w
  in if l2 == 0
     then { r = distance v p, offset = 0 }
     else 
      let t = (dotVec (subVec v p) (subVec v w)) / l2
          r = if t < 0
              then distance p v 
              else 
                if t > 1
                then distance p w
                else
                  let projection = addVec v <| scaleVec t <| subVec v w
                  in distance p projection
          tFix = clamp 0 1 t
      in { r = r, offset = tFix * sqrt l2 }
