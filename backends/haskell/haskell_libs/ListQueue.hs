{-
Queue implementation with an underlying list.
This implementation exposes the data constructor 'Q'.
-}

module ListQueue(
        ListQueue(Q),emptyQueue,queueEmpty,enqueue,dequeue,front) 
where

emptyQueue :: ListQueue a
queueEmpty :: ListQueue a -> Bool
enqueue    :: a -> ListQueue a -> ListQueue a
dequeue    :: ListQueue a -> ListQueue a
front      :: ListQueue a -> a

newtype ListQueue a   = Q [a]
        deriving (Show, Eq)

emptyQueue     = Q []

queueEmpty (Q [])  = True
queueEmpty (Q _ )  = False

enqueue x (Q q)    = Q (q ++ [x])

dequeue (Q (_:xs)) = Q xs
dequeue (Q [])     = error "dequeue: empty queue"

front (Q (x:_)) = x
front (Q [])    = error "front: empty queue"
