module Scene where

import Vector

data Ray = MkRay { origin :: Vec , dir :: Vec }
            deriving Show

mkNormalRay :: Vec -> Vec -> Ray
mkNormalRay o d = MkRay { origin = o, dir = normalize d }



data Plane = MkPlane { point :: Vec , normal :: Vec }


data Object = Sphere Vec Double


type Scene = [Object]


-- intersection of Ray and Object gives a point a normal, ie. a Ray
intersect :: Object -> Ray ->  [Ray]
intersect (Sphere c d) = intersectSphere c d

sq :: Num a => a -> a
sq x = x * x

intersectSphere :: Vec -> Double -> Ray ->  [Ray]
intersectSphere c r (MkRay o d) =
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
              in MkRay p n
        in if disc == 0
             then [normalRay (-b)]
             else [normalRay t1, normalRay t2]






