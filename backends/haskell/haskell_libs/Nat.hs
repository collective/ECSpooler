module Nat where
 
data Nat = Z | S Nat 
    deriving (Eq, Show)
