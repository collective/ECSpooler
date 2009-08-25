{-
################################################################################
#                                Changelog                                     #
################################################################################
#
# 17.04.2009, chbauman:
#       renamed 'test' to 'haskell_backend_internal_equality_test'
-}

--haskell_backend_internal_equality_test :: (Ord a, Fractional a) => a -> a -> Bool
haskell_backend_internal_equality_test x y = (abs (x - y)) < (1 / (10^15))

