module TreeLib where
-- Haskell-Code entnommen aus den Folien, modifizierte Variante benutzt 
-- pattern matching an Stelle von case.

-- Datenstrukturen

data Tree a = Nil | Node a (Tree a) (Tree a)
     deriving (Eq, Ord, Show, Read)

type Table a = [a]

-- direkte Loesung numTree' ---------------------------------------------------

tree2table :: Eq a => Tree a -> Table a
tree2table tree = t2t tree []
  where
    t2t      Nil     table = table
    t2t (Node n l r) table = t2t r (t2t l (update n table))

update :: Eq a => a -> Table a -> Table a
update node table = if elem node table
                       then table
                       else table ++ [node]

lookup' :: (Eq a, Show a) => a -> Table a -> Int
lookup' elem tableG = look elem tableG 0
  where
    look elem   []    _  = error("from lookup': table " ++ show tableG
                                 ++ " does not contain elem " ++ show elem)
    look elem (x:xs) pos = if x == elem then pos
                                        else look elem xs (pos + 1)


numberTree' :: (Eq a, Show a) => Tree a -> Table a -> Tree Int
numberTree' Nil _ = Nil
numberTree' (Node n l r) table = (Node (lookup' n table)
                                       (numberTree' l table)
                                       (numberTree' r table))

numTree' :: (Eq a, Show a) => Tree a -> Tree Int
numTree' tree = numberTree' tree (tree2table tree)

numTree'' :: (Eq a, Show a) => Tree a -> Tree Int
numTree'' tree = ntree
  where
    (ntree, table) = traverseConvert (tree,[])
    traverseConvert (Nil, table) = (Nil, table)
    traverseConvert ((Node n l r), table) = ((Node (lookup' n tabler) nl nr), tabler)
      where
        (nl, tablel) = traverseConvert (l, (update n table))
        (nr, tabler) = traverseConvert (r, tablel)


-- monadische Loesung numTree -------------------------------------------------

data State a b = State (Table a -> (Table a, b))

instance Monad (State a) where
  return x = State (\tab -> (tab,x))
  (State st) >>= f = State (\tab -> let (newTab,y) = st tab
                                        (State trans) = f y
                                        in trans newTab)

extract :: State a b -> b
extract (State st) = snd (st [])

numberNode :: (Show a, Eq a) => a -> State a Int
numberNode x = State (nNode x)

nNode x table
  | elem x table = (table, lookup' x table)
  | otherwise    = (table++[x], length table)

numberTree Nil = return Nil
numberTree (Node x t1 t2) = do num <- numberNode x
                               nt1 <- numberTree t1
                               nt2 <- numberTree t2
                               return (Node num nt1 nt2)

numTree :: (Show a, Eq a) => Tree a -> Tree Int
numTree = extract . numberTree

numberAndMirrorTree Nil = return Nil
numberAndMirrorTree (Node x t1 t2) = do num <- numberNode x
                                        nt1 <- numberAndMirrorTree t1
                                        nt2 <- numberAndMirrorTree t2
                                        return (Node num nt2 nt1)

numAndMirrorTree :: (Show a, Eq a) => Tree a -> Tree Int
numAndMirrorTree = extract . numberAndMirrorTree

-- Beispielbaeume -------------------------------------------------------------
--aTree = 
--    Node "Peter" 
--        (Node "Max" 
--            (Node "John" Nil Nil)
--            (Node "Simon" Nil Nil))
--       (Node "Simon" 
--            (Node "Bill" Nil Nil)
--            (Node "Peter" Nil Nil))

--bTree = 
--    Node "Peter" 
--        (Node "Max" 
--            (Node "John" Nil Nil)
--            (Node "Simon" Nil Nil))
--        (Node "Simon" 
--            Nil
--            (Node "Mike" Nil Nil))
