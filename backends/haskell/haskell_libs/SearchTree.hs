module SearchTree where

data (Ord a) => SearchTree a = Nil | Node a (SearchTree a) (SearchTree a) 
    deriving (Eq, Ord, Show)


