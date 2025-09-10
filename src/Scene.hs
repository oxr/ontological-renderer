module Scene(
  Object (..), 
  Ray (..), 
  Light(..), 
  Scene(..), 
  Cam(..),
  renderScene, 
  twice, spaced, single, showScene, rays, charShades, charNumbers ) where

import Vector



data Ray = Ray Vec Vec 
            deriving Show

origin :: Ray -> Vec
origin (Ray v _ )= v

direction :: Ray -> Vec
direction (Ray _ v) = v

mkNormalRay :: Vec -> Vec -> Ray
mkNormalRay o d = Ray o (normalize d)

data Object = Sphere Vec Double
            | Plane Ray
          deriving Show

newtype Light = Light { mainSource :: Vec }     
          deriving Show

-- at this point light is just a direction from an infinite distance
newtype Scene = Scene { objects:: [Object]  }  deriving Show

data Cam = Cam { dx :: Int , dy :: Int, f :: Int}

{-
multScn :: Double -> Scene -> Scene
multScn n (Scene os) = Scene { objects = map (multO n) os }

multO :: Double -> Object -> Object
multO n (Sphere v r) = Sphere (map (*n) v) (r * n)
multO n (Plane (Ray u v)) = Plane (Ray (map (*n) u) (map (*n) v))

-}

-- intersection of Ray and Object gives a point and a normal, ie. a Ray
intersect :: Object -> Ray ->  [Ray]
intersect (Sphere c d)        = intersectSphere c d
intersect (Plane (Ray o n))   = intersectPlane o n

sq :: Num a => a -> a
sq x = x * x

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


intersectPlane :: Vec -> Vec -> Ray -> [Ray]
intersectPlane o n (Ray ro rd)
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
lightingModel :: Scene -> Light -> Ray -> Double
lightingModel _ (Light ms) = lambert (vecNeg ms)
                where 
                    lambert :: Vec -> Ray -> Double
                    lambert v (Ray _ d)  = v `dot` d


-- now just collect all intersections for all rays , calculate their light value and you are done. 
-- you can also generate a text preview


-- from focal length, x half resolution, y half resolution, give me all rays
-- a matrix of rays
rays :: Int -> Int -> Int -> [[Ray]]
rays f dx dy = [ [ mkRay x y | x <- [-dx .. dx] ] | y <- [-dy .. dy] ]
                where mkRay x y = Ray (V 0 0 0) (V (fromIntegral x) (fromIntegral y) (fromIntegral f))


take1st :: [a] -> [a]
take1st [] = []
take1st (h:_) = [h]

-- an alternative version of minimum, with Maybe, instead of exception
minimum :: (a -> a -> Bool) -> [a] -> [a] 
minimum comp = foldr (\a b -> case b of [] -> [a] ; b':_ -> if a `comp` b' then [a] else [b']) []

minRay :: [Ray] -> [Ray]
minRay = Scene.minimum comp
          where comp (Ray (V _ _ z1) _ ) (Ray (V _ _ z2) _) = z1 < z2 


-- now calculate the light
showScene :: Scene -> Light -> [[Ray]] -> [[Double]]
showScene s l = map showScanline 
                where 
                showScanline  = map (showRay s l)

showRay :: Scene -> Light -> Ray -> Double
showRay s l r = let objectIntersects = map (`intersect`  r) (objects s)
                    flatOI = concat objectIntersects
                    singleIO = minRay flatOI 
                in case singleIO of [] -> 0 ; r' : _ -> lightingModel s l r'

{-
    let objectIntersectors = map intersect (objects s)
                      intersects = [ concatMap take1st [ oi r | oi <- objectIntersectors ,  r <- rs] | rs <- rss ]
                   in map (map (lightingModel s)) intersects
 -}
                                    


renderScene :: [Char] -> (Char -> [Char]) ->  [[Double]] -> [String]
renderScene chars tw = map (concatMap (tw .(chars !!) . levels) )

twice :: a -> [a]
twice x = [x,x]
spaced :: Char -> [Char]
spaced x = [x , ' ']
single :: a -> [a]
single x = [x]



levels :: Double -> Int
levels x | x < -0.9 = 0
         | x < -0.75 = 1
         | x < -0.5 = 2
         | x < -0.2 = 3
         | x < 0    = 4
         | x < 0.2  = 5 
         | x < 0.5  = 6
         | x < 0.7  = 7
         | x < 0.9   = 8
         | otherwise = 9



charShades :: String
charShades = "#@%±*~-,. "

charNumbers :: String
charNumbers = "0123456789"




