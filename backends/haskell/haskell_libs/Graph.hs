module Graph
where
import Array
import Stack
import Queue

mkGraph :: (Ix n,Num w) => Bool -> (n,n) -> [(n,n,w)] -> (Graph n w)
adjacent :: (Ix n,Num w) => (Graph n w) -> n -> [n]
nodes :: (Ix n,Num w) => (Graph n w) -> [n]
edgesU :: (Ix n,Num w) => (Graph n w) -> [(n,n,w)]
edgesD :: (Ix n,Num w) => (Graph n w) -> [(n,n,w)]
edgeIn :: (Ix n,Num w) => (Graph n w) -> (n,n) -> Bool
weight :: (Ix n,Num w) => n -> n -> (Graph n w) -> w

{-- Adjacency matrix representation --}
type Graph n w = Array (n,n) (Maybe w)
            
mkGraph dir bnds@(l,u) es 
    = emptyArray // ([((x1,x2),Just w) |(x1,x2,w)<-es] ++
                     if dir then []
                     else [((x2,x1),Just w) |(x1,x2,w)<-es,x1/=x2])
      where
      emptyArray
          = array ((l,l),(u,u)) [((x1,x2),Nothing) | x1 <- range bnds, 
                                                     x2 <- range bnds]

adjacent g v1 = [ v2 | v2 <-nodes g,(g!(v1,v2))/= Nothing]

nodes g       = range (l,u) where ((l,_),(u,_)) = bounds g

edgeIn g (x,y)= (g!(x,y)) /= Nothing

weight x y g  = w where (Just w) = g!(x,y)

edgesD g       = [(v1,v2,unwrap(g!(v1,v2))) 
                      | v1 <-nodes g, v2 <- nodes g,
                        edgeIn g (v1,v2)]
    where unwrap (Just w) = w

edgesU g       = [(v1,v2,unwrap(g!(v1,v2)))
                   | v1 <-nodes g, v2 <- range (v1,u),
                     edgeIn g (v1,v2)]
    where (_,(u,_)) = bounds g          
          unwrap (Just w) = w

graphAM = mkGraph True (1,5) [(1,2,10),(1,3,20),(2,4,30),(3,4,40),(4,5,50)]

{-- End of Adjacency matrix representation --}

depthFirstSearch start g = dfs [start] []
  where
    dfs [] vis    = vis
    dfs (c:cs) vis 
      | elem c vis = dfs cs vis
      | otherwise  = dfs ((adjacent g c)++cs) (vis++[c])

depthFirstSearch' start g = reverse (dfs [start] [])
  where
   dfs [] vis     = vis
   dfs (c:cs) vis 
      | elem c vis = dfs cs vis
      | otherwise  = dfs ((adjacent g c)++cs) (c:vis)

depthFirstSearch'' start g = reverse (dfs (push start emptyStack) [])
 where
   dfs s vis 
    | (stackEmpty s)  = vis
    | elem (top s) vis = dfs (pop s) vis
    | otherwise       = let c = top s
                        in dfs (foldr push (pop s) (adjacent g c)) (c:vis)

breadthFirstSearch start g = reverse (bfs (enqueue start emptyQueue) [])
 where
  bfs q vis 
   | (queueEmpty q) = vis
   | elem (front q) vis = bfs (dequeue q) vis
   | otherwise  = let c = front q
                  in bfs (foldr enqueue (dequeue q) (adjacent g c))
                          (c:vis)

inDegree g n  = length [t | v<-nodes g, t<-adjacent g v, (n==t)]

topologicalSort g = tsort [n | n<-nodes g , (inDegree g n == 0)] [] 
  where
    tsort [] r      = r
    tsort (c:cs) vis  
     | elem c vis = tsort cs vis
     | otherwise  = tsort cs (c:(tsort (adjacent g c) vis))

