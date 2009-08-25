module TreeOrd where

data TreeOrd a = Nil | Node a (TreeOrd a) (TreeOrd a)  -- Preorder definition
   deriving (Eq, Ord, Show, Read)
