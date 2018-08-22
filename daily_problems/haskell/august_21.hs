-- This problem was asked by Google.
-- 
-- In a directed graph, each node is assigned an uppercase letter. We define a 
-- path's value as the number of most frequently-occurring letter along that 
-- path. For example, if a path in the graph goes through "ABACA", the value of 
-- the path is 3, since there are 3 occurrences of 'A' on the path.
-- 
-- Given a graph with n nodes and m directed edges, return the largest value 
-- path of the graph. If the largest value is infinite, then return null.
-- 
-- The graph is represented with a string and an edge list. The i-th character 
-- represents the uppercase letter of the i-th node. Each tuple in the edge 
-- list (i, j) means there is a directed edge from the i-th node to the j-th 
-- node. Self-edges are possible, as well as multi-edges.
-- 
-- For example, the following input graph:
-- 
-- ABACA
-- 
-- [(0, 1),
--  (0, 2),
--  (2, 3),
--  (3, 4)]
-- 
-- Would have maximum value 3 using the path of vertices [0, 2, 3, 4], (A, A, 
-- C, A).
-- 
-- The following input graph:
-- 
-- A
-- 
-- [(0, 0)]
-- 
-- Should return null, since we have an infinite loop.

import Data.Set (fromList, toList)

allPaths :: (Eq a) => [(a, a)] -> [a] -> Maybe [[a]]
allPaths es vs = do
    res <- mapM (\v -> mkPaths es v []) vs
    return $ concat res

mkPaths :: (Eq a) => [(a, a)] -> a -> [a] -> Maybe [[a]]
mkPaths es s seen = do
    let poss = filter (\(u, v) -> s == u) es
    if null poss
        then return [[s]]
        else do
            ps <- mapM (\(u, v) -> if v `elem` seen 
                                    then Nothing 
                                    else mkPaths es v (s:seen)) poss 
            let res = concat ps
                withS = fmap (\p -> s:p) res
            return withS

mkUnique :: (Ord a) => [a] -> [a]
mkUnique = toList . fromList

longestPath' :: (Ord a) => [(Int, Int)] -> [Int] -> [a] -> Maybe Int
longestPath' es vs ls = do
    allPs <- allPaths es vs
    let seqs = fmap (\p -> fmap (\i -> ls !! i) p) allPs
        counts = fmap (\s -> fmap (\l -> length (filter (== l) s)) (mkUnique s)) seqs
    return $ (maximum . concat) counts

longestPath :: (Ord a) => [(Int, Int)] -> [a] -> Maybe Int
longestPath es vs = longestPath' es [0..((pred . length) vs)] vs

test1 :: IO ()
test1 = do
    let es = [ [ (0, 1)
               , (0, 2)
               , (2, 3)
               , (3, 4)
               ]
             , [ (0, 0)
               ]
             ]
        vs = [ "ABACA"
             , "A"
             ]
        res = fmap (\(e, v) -> longestPath e v) (zip es vs)
    putStrLn $ show res

main :: IO ()
main = do
    test1
