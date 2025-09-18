-- random utility function that for some reason or other I'm defining myself
-- rather than using their predefined versions
module Utils where


isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty (_:_) = False

almost0 :: (Ord a, Fractional a) => a -> Bool
almost0 x = x < 0.0001    
