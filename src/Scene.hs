module Scene where

import Vector


data Ray = Ray Vec Vec 
            deriving Show

origin :: Ray -> Vec
origin (Ray v _ )= v

direction :: Ray -> Vec
direction (Ray _ v) = v


mkNormalRay :: Vec -> Vec -> Ray
mkNormalRay o d = Ray o (normalize d)



data Plane = MkPlane Vec Vec


data Object = Sphere Vec Double

newtype Light = Light { mainSource :: Vec }


-- at this point light is just a direction from an infinite distance
data Scene = Scene { objects:: [Object] , light :: Light } 


-- intersection of Ray and Object gives a point a normal, ie. a Ray
intersect :: Object -> Ray ->  [Ray]
intersect (Sphere c d) = intersectSphere c d

sq :: Num a => a -> a
sq x = x * x

intersectSphere :: Vec -> Double -> Ray ->  [Ray]
intersectSphere c r (Ray o d) =
    let
      oc = o |-| c
      a  = d |.| d
      b  = d |.| oc
      c' = size oc ** 2 - r ** 2
      disc = b*b - a*c'
    in
      if disc < 0 then []
      else
        let sqrtDisc = sqrt disc
            t1 = (-b - sqrtDisc) / a
            t2 = (-b + sqrtDisc) / a
            normalRay t =
              let p = o |+| (t |.*| d)
                  n = normalize (p |-| c)
              in Ray p n
        in if disc == 0
             then [normalRay (-b)]
             else [normalRay t1, normalRay t2]




-- when we collect all intersections, for all rays, 
-- for each intersection which is a colletion of Rays, light model calculates
-- in a given scene, for a given point and direction the 
-- assumptions: Ray is normal , Light vector is normal
lightingModel :: Scene -> Ray -> Double
lightingModel (Scene _ (Light ms)) = lambert (vecNeg ms )
                where 
                    lambert :: Vec -> Ray -> Double
                    lambert v (Ray _ d)  = v `dot` d


-- now just collect all intersections for all rays , calculate their light value and you are done. 
-- you can also generate a text preview


-- from focal length, x half resolution, y half resolution, give me all rays
-- a matrix of rays
rays :: Double -> Double -> Double -> [[Ray]]
rays f dx dy = [ [ mkRay x y | x <- [-dx .. dx] ] | y <- [-dy .. dy] ]
                where mkRay x y = Ray [0,0,0] [x,y,f]


-- one test scene
scene1 :: Scene
scene1 = Scene { objects = [Sphere [0, 0, 100] 25], light = Light { mainSource = normalize [- 1, - 1, 1] } }

take1st :: [a] -> [a]
take1st [] = []
take1st (h:_) = [h]

-- now calculate the light
showScene :: Scene -> [Ray] -> [Double]
showScene s rs = let objectIntersectors = map intersect (objects s)
                     intersects = concatMap take1st [ oi r | oi <- objectIntersectors , r <- rs]
                   in map (lightingModel s) intersects
 
                                    
