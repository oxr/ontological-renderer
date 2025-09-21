module Scene(
  Object (..), 
  Ray (..), 
  Light(..), 
  Scene(..), 
  Shape(..),
  scaleScene, 
) where

import Vector

-- represents a plane
data Ray = Ray { pos :: Vec , dir :: Vec }
            deriving (Show, Eq)

data Object = Object { reflectivity :: Double , shape :: Shape } deriving Show
data Shape = Sphere { center :: Vec, radius :: Double}
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
scaleObject m (Object refl (Sphere c r)) = Object refl (Sphere ( m `scaMult` c) (m * r))
scaleObject m (Object refl (Plane (Ray p n))) = Object refl (Plane (Ray (m `scaMult` p) n ))









