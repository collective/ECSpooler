module Tree where

data Tree a = Nil | Node a (Tree a) (Tree a)  
   deriving (Eq, Show, Read)

depth :: Tree a -> Int
depth Nil = 0
depth (Node n t1 t2) = 1 + max (depth t1)(depth t2)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Nil = Nil
mapTree f (Node n t1 t2) = Node (f n) (mapTree f t1) (mapTree f t2)

collapse :: Tree a -> [a]
collapse Nil = []
collapse (Node n t1 t2) = collapse t1 ++ [n] ++ collapse t2