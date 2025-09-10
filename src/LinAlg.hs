{-# LANGUAGE  MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module LinAlg where

class VecSpace v s | v -> s where
    (*) :: v -> s -> v    -- scalar multiplication
    (+) :: v -> v -> v    -- vector addition
    (-.-) :: v -> v -> s  -- dot product
    zero :: v
    neg :: v -> v         -- negation


class VecSpace v s => Normed v s where
    norm :: v -> s        -- norm




    


