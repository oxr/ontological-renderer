module Scene where

import Vector

data Ray = MkRay { origin :: Vec , dir :: Vec }

type Plane = Ray

