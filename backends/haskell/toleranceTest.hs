--test :: (Ord a, Fractional a) => a -> a -> Bool
test x y = (abs (x - y)) < (1 / (10^15))

