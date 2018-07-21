{-# LANGUAGE KindSignatures #-}

import qualified Data.Map as M
import Data.List (sortOn)

class PriorityQueue (m :: * -> *) where
    pop :: Ord a => m a -> (a, (m a))
    push :: Ord a => m a -> a -> m a
    mkEmpty :: m a
    isEmpty :: Ord a => m a -> Bool

instance PriorityQueue ([]) where
    pop q = (v, newQ)
        where
            v = minimum q
            newQ = filter (/= v) q
    push pq a = a:pq
    mkEmpty = []
    isEmpty = null

-- PSEUDOCODE: 
--
-- pred <- { a : None for a in verts }      // denote the immediante predecessor of each node
-- dist <- { a : inf for a in verts }       // denote the distance from the source node to every other node.
-- 
--
-- q <- insert src empty                    // insert the source node as the only node with non-inf dist in the q.
--
-- for v in verts if v is not src:
--      q <- insert q (v, dist[v])
--
-- while not (isEmpty q):
--      closest <- pop q                    // the closest node that we know about
--      nextHops <- neighbours closest      // Nodes seperated from closest by a single hop [(NodeId, EdgeWeight)]
--
--      for (id, w) in nextHops:
--          if dist[closest] + w < dist[id]:
--              dist[id] = dist[closest] + w
--              pred[id] = closest
--          
-- return (dist, pred)
--
firstSatisfying :: (a -> Bool) -> [a] -> a
firstSatisfying pred as = res
    where
        poss = filter pred as
        res = head poss

at :: [(Int, Int)] -> Int -> Int
ds `at` v = res
    res = head $ filter (\(k, _) -> k == v) ds
    

dijkstra :: [(Int, Int)] -> [(Int, Int)] -> [Int] -> [(Int, (Int, Int))] -> ([(Int, Int)], [(Int, Int)])
dijkstra dist pred [] _ = (dist, pred)
dijkstra dist pred pq es = ([], [])
    where
        s = sortOn (\v -> snd (firstSatisfying (\a -> (fst a) == v) dist)) pq
        minNode = head s
        neighbours = filter (\(v, _) -> v == minNode) es
        toUpdate =  









        
