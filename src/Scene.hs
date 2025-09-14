module Scene(
  Object (..), 
  Ray (..), 
  Light(..), 
  Scene(..), 
  Cam(..),
  renderScene, 
  twice, spaced, single, showScene, rays, charShades, charNumbers, intersect, minRay ) where

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

data Cam = Cam { cam_dx :: Int , cam_dy :: Int, cam_f :: Int}

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
lightingModel s (Light ms) p = 
  if pointIsLit then lambert (vecNeg ms) p else -1 + reflect
    where 
      pointIsLit :: Bool -- is the point p visible from light ? 
      pointIsLit = isEmpty (objectIntersects (Ray (pos p) (ms)))
      objectIntersects :: Ray -> [Double]
      objectIntersects r =  filter (\x -> not (almost0 x) && x > 0) (map (dot ms . (pos r <-> ) . pos) (concatMap (`intersect` r ) (objects s)))
      lambert :: Vec -> Ray -> Double
      lambert v (Ray _ d)  = v `dot` d
      reflect :: Double
      reflect = 0 --(normalize (dir p) `dot` normalize(ms)) 

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty (_:_) = False

almost0 :: (Ord a, Fractional a) => a -> Bool
almost0 x = x < 1e-12    


-- now just collect all intersections for all rays , calculate their light value and you are done. 
-- you can also generate a text preview


-- from focal length, x half resolution, y half resolution, give me all rays
-- a matrix of rays
rays :: Int -> Int -> Int -> [[Ray]]
rays f dx dy = [ [ mkRay x y | x <- [-dx .. dx] ] | y <- [-dy .. dy] ]
                where mkRay x y = Ray (V 0 0 0) (V (fromIntegral x) (fromIntegral y) (fromIntegral f))

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
showRay s l r = let oi = map (`intersect`  r) (objects s)
                    flatOI = concat oi
                    singleIO = minRay flatOI 
                in case singleIO of 
                    []      -> -1 
                    r' : _  -> lightingModel s l r'

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
charShades = "#%±=:;-,. "

charNumbers :: String
charNumbers = "0123456789"




