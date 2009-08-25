module SetKreise where

newtype Set a = St [a]

emptySet :: Set a       
emptySet = St []

setEmpty :: Set a -> Bool            
setEmpty (St []) = True
setEmpty _ = False

inSet :: Eq a => a -> Set a -> Bool
inSet x (St xs) = elem x xs

addSet :: a -> Set a -> Set a 
addSet x (St xs) = St (x:xs)

delSet :: Eq a => a -> Set a -> Set a
delSet x (St xs) = St (delete x xs) where
  delete _ [] = []
  delete x (y:ys) = if x == y then ys else y : delete x ys

subSet :: Eq a => Set a -> Set a -> Bool
subSet (St xs) (St ys) = all (`elem` ys) xs

eqSet :: Eq a => Set a -> Set a -> Bool
eqSet set1 set2 = subSet set1 set2 && subSet set2 set1

-- Auf Mengen uebertragene Listenfunktionen --

filterSet :: (a -> Bool) -> Set a -> Set a
filterSet p (St xs) = St (filter p xs)

mapSet :: (a -> b) -> Set a -> Set b
mapSet f (St xs) = St (map f xs)

allSet :: (a -> Bool) -> Set a -> Bool
allSet p (St xs) = all p xs

anySet :: (a -> Bool) -> Set a -> Bool
anySet p (St xs) = any p xs

setToList (St xs) = xs
-- Anzeigefunktionen --

showSet []     str = showString "{}" str
showSet (x:xs) str = showChar '{' ( shows x ( showl xs str))
     where showl []     str = showChar '}' str
           showl (x:xs) str = showChar ',' (shows x (showl xs str))

instance (Show a) => Show (Set a) where
    showsPrec _ (St s) str = showSet s str
