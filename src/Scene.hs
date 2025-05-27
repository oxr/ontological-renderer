module Scene where

import Vector

data Ray = MkRay { origin :: Vec , dir :: Vec }

data Plane = MkPlane { point :: Vec , normal :: Vec }

 