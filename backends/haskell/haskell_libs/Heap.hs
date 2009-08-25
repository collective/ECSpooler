module Heap
    where 

emptyHeap:: (Ord a) => Heap a
heapEmpty:: (Ord a) => Heap a -> Bool
findHeap :: (Ord a) => Heap a -> a
insHeap  :: (Ord a) => a -> Heap a -> Heap a
delHeap  :: (Ord a) => Heap a -> Heap a

-- IMPLEMENTATION with Leftist Heap
-- adapted from C. Okasaki Purely Functional Data Structures p 197

data (Ord a) => Heap a = EmptyHP
                       | HP a Int (Heap a) (Heap a)
    deriving (Eq,Show)

emptyHeap = EmptyHP

heapEmpty EmptyHP = True
heapEmpty _       = False
                    
findHeap EmptyHP      = error "findHeap:empty heap"
findHeap (HP x _ a b) = x

insHeap x h = merge (HP x 1 EmptyHP EmptyHP) h

delHeap EmptyHP      = error "delHeap:empty heap"
delHeap (HP x _ a b) = merge a b

-- auxiliary functions

rank :: (Ord a) => Heap a -> Int
rank EmptyHP      = 0
rank (HP _ r _ _) = r
                    
makeHP :: (Ord a) => a -> Heap a -> Heap a -> Heap a
makeHP x a b | rank a >= rank b = HP x (rank b + 1) a b
             | otherwise        = HP x (rank a + 1) b a

merge ::(Ord a) =>  Heap a -> Heap a -> Heap a
merge h EmptyHP = h
merge EmptyHP h = h
merge h1@(HP x _ a1 b1) h2@(HP y _ a2 b2)
      | x <= y    = makeHP x a1 (merge b1 h2)
      | otherwise = makeHP y a2 (merge h1 b2)

-- examples of use of auxiliary functions

fig5_5a = insHeap 6 (insHeap 1(insHeap 4 (insHeap 8 emptyHeap)))
-- HP 1 2 (HP 4 1 (HP 8 1 EmptyHP EmptyHP) EmptyHP) 
--        (HP 6 1 EmptyHP EmptyHP)

fig5_5b = insHeap 7 (insHeap 5 emptyHeap)
-- HP 5 1 (HP 7 1 EmptyHP EmptyHP) EmptyHP

{- examples of calls and results
Heap> merge fig5_5a fig5_5b
HP 1 2 (HP 5 2 (HP 7 1 EmptyHP EmptyHP) (HP 6 1 EmptyHP EmptyHP)) 
       (HP 4 1 (HP 8 1 EmptyHP EmptyHP) EmptyHP)
-}
