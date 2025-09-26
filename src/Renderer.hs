{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Renderer (
  Cam(..),
  Shape(..),
  typewriterDoubles,
  twice,
  spaced,
  single,
  showScene,
  showRay,
  rays,
  charShades,
  charNumbers,
  minRay,
  intersect,
  reflection
) where

import Scene
import Vector
import Utils
import Data.List (elemIndex)
import Data.Bifoldable (bifoldl')
import GHC.Base (VecElem(Word16ElemRep))

data Cam = Cam {
    cam_pos :: Vec ,
    cam_dir :: Vec ,
    resolution_x :: Int ,
    resolution_y :: Int,
    focalLength :: Int}


-- refraction of incomming ray, at a point defined by the second argument
reflection :: Vec -> Ray -> Ray
reflection  rd (Ray op on) = Ray op refdir
                                   where nr = rd -- normalize rd
                                         nn = on -- normalize on
                                         refdir = nr <-> (2 * (nr `dot` nn)) <^*> nn

-- intersection of Ray and Object gives a point and a normal, ie. a Ray
intersect :: Shape -> Ray ->  [Ray] -- Ray is the plane, Vec is refraction
intersect (Sphere c d)  = intersectSphere c d
intersect (Plane r')    = intersectPlane r'

intersectSphere :: Vec -> Double -> Ray ->  [Ray]
intersectSphere c r (Ray o d) =
    let
      oc = o <-> c
      a  = d <.> d
      b  = d <.> oc
      c' = size oc ** 2 - r ** 2
      disc = b*b - a*c'
    in
      if disc < 0 then []
      else
        let sqrtDisc = sqrt disc
            t1 = (-b - sqrtDisc) / a
            t2 = (-b + sqrtDisc) / a
            normalRay t =
              let p = o <+> (t <^*> d)
                  n = normalize (p <-> c)
              in Ray p n
        in if disc == 0
             then [normalRay (-b)]
             else [normalRay t1, normalRay t2]

intersectPlane :: Ray -> Ray -> [Ray]
intersectPlane (Ray o n) (Ray ro rd)
  | denom == 0 = []  -- Ray is parallel to the plane
  | t < 0      = []  -- Intersection is behind the ray origin
  | otherwise  = [Ray p n]  -- Intersection point with plane normal
  where
    denom = dot n rd
    t     = dot n (o <-> ro) / denom
    p     = ro <+> rd <*^> t





-- when we collect all intersections, for all rays, 
-- for each intersection which is a collection of Rays, light model calculates
-- in a given scene, for a given point and direction the 
-- assumptions: Ray is normal , Light vector is normal
lightingModel :: Scene -> Light -> Ray -> Colour
lightingModel s (Light ms) p =
  if pointIsLit then <*^> (round $ 255 *  lambert (vecNeg (normalize ms)) p) else (0,0,0, max ( round $ 255 * reflect * 0.5) 0)
    where
      pointIsLit :: Bool -- is the point p visible from light ? 
      pointIsLit = null $ objectIntersects (Ray (pos p) ms)
      objectIntersects :: Ray -> [Double]
      objectIntersects r =  filter (\x -> not (almost0 x) && x > 0)
                            (map (dot ms . (pos r <-> ) . pos) (concatMap ((`intersect` r) . shape) (objects s)))
      lambert :: Vec -> Ray -> Double
      lambert v (Ray _ d)  = min (normalize v `dot` normalize d) 1
      reflect :: Double
      reflect =  dir p `dot` ms




-- from focal length, x half resolution, y half resolution, give me all rays
-- a matrix of rays
rays :: Int -> Int -> Int -> [[Ray]]
rays f dx dy = [ [ mkRay x y | x <- [-dx .. dx] ] | y <- [-dy .. dy] ]
                where mkRay x y = Ray (V 0 0 0) (V (fromIntegral x) (fromIntegral y) (fromIntegral f))



newtype Distanced a b = Dist { unD :: (a, b) }
instance Eq a => Eq (Distanced a b) where
  (==) (Dist (a,b)) (Dist (a',b')) =  a' == a'


instance Ord a => Ord (Distanced a b) where
  a <= b  = fst (unD a) <= fst (unD b)




-- finds the ray that is nearest to origin of the Ray, on the plus side of the ray
minRay :: HasPosition a => Ray -> [a] -> Maybe a
minRay (Ray ro rd) vs = let dar =
                              filter ( (>= 0.00002) . fst . unD )
                              (map (\r -> Dist ((pos r <-> ro) `dot` rd, r)) vs)-- distances along the ray
                            minray = if null dar then Nothing else Just (minimum dar)
                        in Just . snd . unD =<< minray

instance HasPosition a => HasPosition (a,b) where
  pos (a,_) = pos a



-- now calculate the light
showScene :: Scene -> Light -> Int -> [[Ray]] -> [[Cmyk]]
showScene s l d = map (map (showRay s l d))

fromDouble :: Double -> Int
fromDouble = round . (* 255)

shade i = (0,0,0,i)


showRay :: Scene -> Light -> Int -> Ray -> Colour
showRay s l depth r = let oi = map intersections (objects s) -- each objects results in a list of intersections
                          singleIO = minRay r (concat oi) -- from these we choose the upfront one
                      in case singleIO of
                        Nothing         -> shade (fromDouble 0.75) -- the colour of the sky 
                        Just (r', (ri, _))   -> if depth <= 0 then lightingModel s l r'
                                           else (fromDouble ri, fromDouble ri, fromDouble ri , fromDouble ri)
                                                  *
                                                showRay s l (depth -1) (reflection (dir r) r') + quadruply (fromDouble $ 1-ri) * lightingModel s l r'
                      where intersections :: Object -> [(Ray, Surface)] -- find intersection and stick reflectivity information to it
                            intersections (Object o face) = map (,face) (o `intersect`  r)

typewriterDoubles :: [Char] -> (Char -> [Char]) ->  [[Double]] -> [String]
typewriterDoubles chars tw = map (concatMap (tw .(chars !!) . levels) )

twice :: a -> [a]
twice x = [x,x]
spaced :: Char -> [Char]
spaced x = [x , ' ']
single :: a -> [a]
single x = [x]

levels :: Double -> Int
levels x | x < -0.5 = 0
         | x < -0.4 = 1
         | x < -0.3 = 2
         | x < -0.15 = 3
         | x < 0    = 4
         | x < 0.15  = 5
         | x < 0.3  = 6
         | x < 0.4  = 7
         | x < 0.5   = 8
         | otherwise = 9



charShades :: String
charShades = "#%±+/=,.  "

charNumbers :: String
charNumbers = "0123456789"

