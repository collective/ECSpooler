{-
################################################################################
#                                Changelog                                     #
################################################################################
#
# 17.04.2009, chbauman:
#       renamed 'test' to 'haskell_backend_internal_equality_test'
#       gave all other methods 'haskell_backend_internal_' as prefix due to scope issues
-}

--haskell_backend_internal_equality_test :: Eq a => [a] -> [a] -> Bool
haskell_backend_internal_equality_test a b = haskell_backend_internal_test_ispermutation a b

haskell_backend_internal_remove item [] = []
haskell_backend_internal_remove item (head:tail)
    | head == item = tail
    | otherwise = (head:(haskell_backend_internal_remove item tail) )

haskell_backend_internal_member item [] = False
haskell_backend_internal_member item (head:tail)
    | head == item = True
    | otherwise = (haskell_backend_internal_member item tail)

haskell_backend_internal_test_ispermutation [] [] = True
haskell_backend_internal_test_ispermutation x  [] = False
haskell_backend_internal_test_ispermutation [] x  = False
haskell_backend_internal_test_ispermutation (head:tail) list2
    | (haskell_backend_internal_member head) list2 == True = haskell_backend_internal_test_ispermutation tail (haskell_backend_internal_remove head list2)
    | otherwise = False
