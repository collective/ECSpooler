module RBTree where

data Color = R|B
    deriving (Eq, Show)

data (Ord a) => RBTree a = E | T Color (RBTree a) a (RBTree a)
    deriving (Eq, Show)

