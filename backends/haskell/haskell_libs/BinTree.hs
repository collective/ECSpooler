module BinTree 
where

inTree    :: (Ord a,Show a) => a -> BinTree a -> Bool
addTree   :: (Ord a,Show a) => a -> BinTree a -> BinTree a
delTree   :: (Ord a,Show a) => a -> BinTree a -> BinTree a
buildTree :: (Ord a,Show a) => [a] -> BinTree a
inorder   :: (Ord a,Show a) => BinTree a -> [a]

data (Ord a) => BinTree a = EmptyBT
                          | NodeBT a (BinTree a) (BinTree a)
    deriving Show

emptyTree = EmptyBT

inTree v' EmptyBT                  = False
inTree v' (NodeBT v lf rt) | v==v' = True  
                           | v'<v  = inTree v' lf
                           | v'>v  = inTree v' rt

addTree v' EmptyBT                      = NodeBT v' EmptyBT EmptyBT
addTree v' (NodeBT v lf rt) | v'==v     = NodeBT v lf rt
                            | v' < v    = NodeBT v (addTree v' lf) rt
                            | otherwise = NodeBT v lf (addTree v' rt)
                                        
buildTree lf = foldr addTree EmptyBT  lf

-- value not found
delTree v' EmptyBT                       = EmptyBT 
-- one descendant
delTree v' (NodeBT v lf EmptyBT) | v'==v = lf 
delTree v' (NodeBT v EmptyBT rt) | v'==v = rt
-- two descendants
delTree v' (NodeBT v lf rt)
    | v'<v  = NodeBT v (delTree v' lf) rt 
    | v'>v  = NodeBT v lf (delTree v' rt)  
    | v'==v = let k = minTree rt 
                  in NodeBT k lf (delTree k rt)

minTree (NodeBT v EmptyBT _) = v
minTree (NodeBT _ lf _)      = minTree lf 

inorder EmptyBT = []
inorder (NodeBT v lf rt) = inorder lf ++ [v] ++ inorder rt

