{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Scene(
  Object (..), 
  Ray (..), 
  Light(..), 
  Scene(..), 
  Shape(..),
  scaleScene, 
  Cmyk,Colour,Rgb,
  Surface
) where

import Vector

-- represents a plane
data Ray = Ray { position :: Vec , direction :: Vec }
            deriving (Show, Eq)
                                
type Cmyk = (Int, Int, Int, Int)
type Rgb = Vec_ Int
type Colour = Rgb
type Surface = (Double , Colour)



data Object = Object { shape :: Shape , face :: Surface} deriving Show
data Shape = Sphere { center :: Vec, radius :: Double}
           | Plane { normal :: Ray}
          deriving Show

newtype Light = Light { mainSource :: Vec }     
          deriving Show

-- at this point light is just a direction from an infinite distance
newtype Scene = Scene { objects:: [Object]  }  deriving Show

instance Num a => Num (a,a,a,a) where
  (a,b,c,d) * (a',b',c',d') = (a*a' , b*b', c*c', d*d')
  (+) = quadLift2 (+)
  abs = quadLift abs
  

quadmap f (a,b,c,d) = (f a , f b, f c , f d) 

quadLift :: (a -> a) -> (a,a,a,a) -> (a,a,a,a)
quadLift = quadmap


quadLift2 :: (a -> a -> a) -> (a,a,a,a) -> (a,a,a,a) -> (a,a,a,a)
quadLift2 f (a,b,c,d) (a',b',c',d') = (f a a', f b b', f c c', f d d')

quadruply a = (a,a,a,a)

instance Fractional a => Fractional (a,a,a,a) where 
    
  

instance HasPosition Ray  where
  pos = position

instance HasDirection Ray  where
  dir = direction



instance HasPosition Shape where
  pos (Sphere c _ ) = c
  pos (Plane (Ray p _)) = p

instance HasPosition Object where
  pos (Object s _ ) = pos s

-- matrix multiplication for scenes
scaleScene :: Double -> Scene -> Scene
scaleScene m (Scene os) = Scene (map (scaleObject m) os)

scaleObject :: Double -> Object -> Object
scaleObject m (Object  (Sphere c r) sface) = Object (Sphere ( m `scaMult` c) (m * r)) sface
scaleObject m (Object  (Plane (Ray p n)) sface) = Object (Plane (Ray (m `scaMult` p) n )) sface


