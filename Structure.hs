
{-# Options -Wall -Werror #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Structure where

import TaggedRoseTree

import Data.Vinyl

{- Basic Fields -}

length :: "length" ::: Double
length = Field

width :: "width" ::: Double
width = Field

radius :: "radius" ::: Double
radius = Field

offset :: "offset" ::: Double
offset = Field

angle :: "angle" ::: Double
angle = Field

group :: "group" ::: Int
group = Field

{- Structure Types -}

data Part
  = Engine (PlainRec '[ "radius" ::: Double, "group" ::: Int ])
  | FuelTank (PlainRec [ "length" ::: Double, "width" ::: Double ])
  | Processor (PlainRec '[ "radius" ::: Double ])

type Beam = PlainRec '[ "length" ::: Double ]
type Attach = PlainRec [ "offset" ::: Double, "angle" ::: Double ]

type Structure = TaggedRoseTree Part Beam Attach

{- Vectors -}

x :: "x" ::: Double
x = Field

y :: "y" ::: Double
y = Field

type Vec2 = PlainRec [ "x" ::: Double, "y" ::: Double ]

scaleVec :: 
  (IElem ("x" ::: Double) v, IElem ("y" ::: Double) v)
  => Double -> PlainRec v -> PlainRec v
scaleVec a = rMod x (* a) . rMod y (* a)

rotVec ::
  (IElem ("x" ::: Double) v, IElem ("y" ::: Double) v)
  => Double -> PlainRec v -> PlainRec v
rotVec theta vec = rPut x vx' . rPut y vy' $ vec where
  vx = rGet x vec
  vy = rGet y vec
  vx' = vx * cos theta - vy * sin theta
  vy' = vx * sin theta + vy * cos theta

addVec :: 
  (IElem ("x" ::: Double) v1, IElem ("y" ::: Double) v1,
  IElem ("x" ::: Double) v2, IElem ("y" ::: Double) v2)
  => PlainRec v1 -> PlainRec v2 -> PlainRec v2
addVec vec =
  rMod x (+ rGet x vec) 
  . rMod y (+ rGet y vec)

sumVectors :: [Vec2] -> Vec2
sumVectors = foldr addVec (x=:0<+>y=:0)

translateAttach :: 
  (IElem ("x" ::: Double) v, IElem ("y" ::: Double) v)
  => Attach -> PlainRec v -> PlainRec v
translateAttach attach = addVec $ x=:dX <+> y=:dY

  where

  dX :: Double
  dX = rGet offset attach * (cos $ rGet angle attach)

  dY :: Double
  dY = rGet offset attach * (sin $ rGet angle attach)

{- Getting the Center of Mass -}

mass :: "mass" ::: Double
mass = Field

type PointMass = PlainRec '[
  "x" ::: Double,
  "y" ::: Double,
  "mass" ::: Double]

sumMasses :: [PointMass] -> Double
sumMasses = sum . fmap (rGet mass)

massContrib :: PointMass -> Vec2
massContrib pm = 
  x =: (rGet x pm * rGet mass pm) <+>
  y =: (rGet y pm * rGet mass pm)

partMasses :: Part -> [PointMass]
partMasses _part = [x=:0 <+> y=:0 <+> mass=:1]

beamMasses :: Beam -> [[PointMass]] -> [PointMass]
beamMasses _beam subMasses = concat subMasses

attachMasses :: Attach -> [PointMass] -> [PointMass]
attachMasses attach subMasses = 
  fmap (translateAttach attach) subMasses 


structureMasses :: Structure -> [PointMass]
structureMasses = foldTaggedRoseTree partMasses beamMasses attachMasses

centerOfMass :: Structure -> PointMass
centerOfMass struct = normalize . sumVectors $ contribs 

  where
  
  flattenedMasses :: [PointMass]
  flattenedMasses = structureMasses struct

  contribs :: [Vec2]
  contribs = fmap massContrib flattenedMasses

  totalMass :: Double
  totalMass = sumMasses flattenedMasses

  normalize :: Vec2 -> PointMass
  normalize = 
    (<+> mass=:totalMass ) 
    . rMod x (/totalMass) 
    . rMod y (/totalMass)

{- Thrust Types -}

pos :: "pos" ::: Vec2
pos = Field

force :: "force" ::: Vec2
force = Field

type Thrust = PlainRec '[ "pos" ::: Vec2, "force" ::: Vec2, "group" ::: Int ]

partThrusts :: Part -> [Thrust]
partThrusts part = case part of
  Engine meta -> 
    let p = x=:0 <+> y=:0
        f = x=:0 <+> y=:0
        g = rGet group meta
    in [pos=:p <+> force=:f <+> group=:g]
  _ -> []

beamThrusts :: Beam -> [[Thrust]] -> [Thrust]
beamThrusts _ = concat

attachThrusts :: Attach -> [Thrust] -> [Thrust]
attachThrusts attach = fmap attachThrust where
  attachThrust = rMod pos (translateAttach attach) . rMod force (rotVec $ rGet angle attach)

structureThrusts :: Structure -> [Thrust]
structureThrusts = foldTaggedRoseTree partThrusts beamThrusts attachThrusts
