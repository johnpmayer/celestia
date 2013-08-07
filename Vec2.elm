module Vec2 where

{- 2D Vector -}

type Vec2Ext a = { a | x : Float, y : Float }

type Vec2 = Vec2Ext {}

origin : Vec2
origin = { x = 0, y = 0 }

scaleVec : Float -> Vec2Ext a -> Vec2Ext a
scaleVec a v = { v | x <- v.x * a, y <- v.y * a }

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

flip : (a -> b -> c) -> (b -> a -> c)
flip f a b = f b a

sumVec : [ Vec2Ext a ] -> Vec2
sumVec = foldl (flip addVec) origin
