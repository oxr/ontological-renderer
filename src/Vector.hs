module Vector where

type Vec = [Double]

dot :: Vec -> Vec -> Double
dot x y = sum (zipWith (*) x y)

size :: Vec -> Double
size = sqrt . sum . map (\x -> x * x) 
normalize :: Vec -> [Double]
normalize x  = map (/s)  x      where s = size x

cross :: Num a => [a] -> [a] -> [a]
cross [a1,a2,a3] [b1,b2,b3] = [a2*b3 - a3*b2, a3*b1-a1*b3,a1*b2-a2*b1]
cross _ _                   = []

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

