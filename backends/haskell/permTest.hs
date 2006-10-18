test a b = test_ispermutation a b

remove item [] = []
remove item (head:tail)
    | head == item = tail
    | otherwise = (head:(remove item tail) )

member item [] = False
member item (head:tail)
    | head == item = True
    | otherwise = (member item tail)

test_ispermutation [] [] = True
test_ispermutation x  [] = False
test_ispermutation [] x  = False
test_ispermutation (head:tail) list2
    | (member head) list2 == True = test_ispermutation tail (remove head list2)
    | otherwise = False
