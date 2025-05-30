module Vector where


type Vec = [Double]

dot :: Vec -> Vec -> Double
dot x y = sum (zipWith (*) x y)

( <.> ) :: Vec -> Vec -> Double
( <.> ) = dot
infixl 7 <.>


size :: Vec -> Double
size = sqrt . sum . map (\x -> x * x)
normalize :: Vec -> [Double]
normalize x  = map (/s)  x      where s = size x


cross :: Num a => [a] -> [a] -> [a]
cross [a1,a2,a3] [b1,b2,b3] = [a2*b3 - a3*b2, a3*b1-a1*b3,a1*b2-a2*b1]
cross _ _                   = []

(<*>) :: Num a => [a] -> [a] -> [a]
(<*>) = cross
infixl 7 <*>

normal :: [Double] -> [Double] -> [Double]
normal a b = normalize (cross a b)


sinVec :: Vec -> Vec -> Double
sinVec a b = size (a `cross` b) / size a / size b

cosVec :: Vec -> Vec -> Double
cosVec a b = dot a b  / size a * size b


-- matrixes are assumed to be rectangular
-- lines are vectors
type Matrix = [Vec] 

transpose :: Matrix -> Matrix
-- transpose [] = []
transpose ([] : _) = []
transpose xss = map head xss : transpose (map tail xss)

vecAdd :: Vec -> Vec -> Vec
vecAdd = zipWith (+)

(<+>) :: Vec -> Vec -> Vec
(<+>) = vecAdd

infixl 6 <+>

-- scalar multiplication
scaMult :: Num b => b -> [b] -> [b]
scaMult s = map (* s)

(<^*>) :: Num b => b -> [b] -> [b]
(<^*>) = scaMult

(<*^>) :: Num b => [b] -> b -> [b]
(<*^>) = flip scaMult 

infixl 9 <*^>, <^*>


vecNeg :: Vec -> Vec
vecNeg = ((-1) <^*> )

(<->) :: Vec -> Vec -> Vec
u <-> v = u <+> vecNeg v

infixl 6 <->
