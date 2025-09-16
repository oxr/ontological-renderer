module Scene(
  Object (..), 
  Ray (..), 
  Light(..), 
  Scene(..), 
  scaleScene ) where

import Vector

data Ray = Ray { pos :: Vec , dir :: Vec }
            deriving Show

data Object = Sphere { center :: Vec, radius :: Double}
            | Plane { normal :: Ray}
          deriving Show

newtype Light = Light { mainSource :: Vec }     
          deriving Show

-- at this point light is just a direction from an infinite distance
newtype Scene = Scene { objects:: [Object]  }  deriving Show


-- matrix multiplication for scenes
scaleScene :: Double -> Scene -> Scene
scaleScene m (Scene os) = Scene (map (scaleObject m) os)

scaleObject :: Double -> Object -> Object
scaleObject m (Sphere c r) = Sphere (m `scaMult` c) (m * r)
scaleObject m (Plane (Ray p n)) = Plane (Ray (m `scaMult` p) n )








