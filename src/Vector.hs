{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE Rank2Types #-}


module Vector where

data Nat = Zero | Succ Nat

data Vec_ a = V a a a deriving (Eq, Show)
type Vec = Vec_ Double

instance Foldable Vec_  where 
    foldr :: (a -> b -> b) -> b -> Vec_ a -> b
    foldr f b (V x y z) = f x (f y (f z b))

instance Functor Vec_ where
    fmap f (V a b c) = V (f a) (f b) (f c)



zipWithVec :: (a -> b -> c) -> Vec_ a -> Vec_ b -> Vec_ c
zipWithVec f (V a b c) (V a' b' c')  = (V (f a a') (f b b') (f c c'))

zipV :: Vec_ a -> Vec_ b -> Vec_ (a,b)
zipV = zipWithVec (,)

dot :: Num a => Vec_ a -> Vec_ a -> a
dot x y = sum (zipWithVec (*) x y)

(<.>) :: forall a. Num a => Vec_ a -> Vec_ a -> a
( <.> ) = dot
infixl 7 <.>

size :: (Floating a, Num a) => Vec_ a -> a
size v = sqrt (v <.> v)

normalize :: (Floating a, Num a) => Vec_ a -> Vec_ a
normalize x  = fmap (/s)  x      
                where s = size x

cross :: Num a => Vec_ a -> Vec_ a -> Vec_ a
cross (V a1 a2 a3) (V b1 b2 b3) = (V (a2*b3 - a3*b2) (a3*b1-a1*b3) (a1*b2-a2*b1))

(<*>) :: Num a => Vec_ a -> Vec_ a -> Vec_ a
(<*>) = cross
infixl 7 <*>

normal :: (Monoid a , Floating a)=>  Vec_ a -> Vec_ a -> Vec_ a
normal a b = normalize (cross a b)

sinVec :: Vec  -> Vec  -> Double
sinVec a b = size (a `cross` b) / size a / size b

cosVec :: Vec -> Vec -> Double
cosVec a b = dot a b  / size a * size b

-- matrixes are assumed to be rectangular
-- lines are vectors
type Matrix_ a = Vec_ (Vec_ a)

transpose :: Matrix_ a -> Matrix_ a
transpose (V (V a11 a12 a13 ) (V a21 a22 a23) (V a31 a32 a33)) = 
            (V (V a11 a21 a31) (V a12 a22 a32) (V a13 a23 a33))

-- vector +
(<+>) :: Num a => Vec_ a -> Vec_ a  -> Vec_ a
(<+>) = zipWithVec (+) 

infixl 6 <+>

-- scalar multiplication
scaMult :: Num a => a -> Vec_ a -> Vec_ a
scaMult a = fmap (* a)

(<^*>) :: Num b => b -> Vec_ b -> Vec_ b
(<^*>) = scaMult

(<*^>) :: Num b => Vec_ b -> b -> Vec_ b
(<*^>) = flip scaMult 

infixl 9 <*^>, <^*>


vecNeg :: Num a => Vec_ a -> Vec_ a
vecNeg = scaMult (-1)

-- vector minus
(<->) :: Vec -> Vec -> Vec
u <-> v = u <+> vecNeg v

infixl 6 <->
