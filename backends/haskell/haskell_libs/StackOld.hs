module Stack where

data Stack = NIL | Ele Int Stack
    deriving (Eq, Show)

