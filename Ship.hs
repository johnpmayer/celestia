
{-# Options -Wall -Werror #-}
{-# LANGUAGE DataKinds, TypeOperators #-}

module Ship where

import TaggedRoseTree

import Data.Vinyl

{- Bunch of fields used -}

length :: "length" ::: Double
length = Field :: "length" ::: Double
width :: "width" ::: Double
width = Field :: "width" ::: Double
radius :: "radius" ::: Double
radius = Field :: "radius" ::: Double
offset :: "offset" ::: Double
offset = Field :: "offset" ::: Double
angle :: "angle" ::: Double
angle = Field :: "angle" ::: Double
x :: "x" ::: Double
x = Field :: "x" ::: Double
y :: "y" ::: Double
y = Field :: "y" ::: Double
mass :: "mass" ::: Double
mass = Field :: "mass" ::: Double

{- Structure definition -}

data Part
  = Engine (PlainRec '[ "radius" ::: Double ])
  | FuelTank (PlainRec [ "length" ::: Double, "width" ::: Double ])
  | Processor (PlainRec '[ "radius" ::: Double ])

type Beam = PlainRec '[ "length" ::: Double ]
type Attach = PlainRec [ "offset" ::: Double, "angle" ::: Double ]

type Structure = TaggedRoseTree Part Beam Attach

{- Getting the Center of Mass -}

type Vec2 = PlainRec [ "x" ::: Double, "y" ::: Double ]

addVec :: Vec2 -> Vec2 -> Vec2
addVec v1 v2 =
  x =: (rGet x v1 + rGet x v2) <+>
  y =: (rGet y v1 + rGet y v2)

sumVectors :: [Vec2] -> Vec2
sumVectors = foldr addVec (x=:0<+>y=:0)

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
attachMasses attach subMasses = fmap trans subMasses 

  where

  dX :: Double
  dX = rGet offset attach * (cos $ rGet angle attach)

  dY :: Double
  dY = rGet offset attach * (sin $ rGet angle attach)

  trans :: PointMass -> PointMass
  trans = rMod x (+dX) . rMod y (+dY)

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


