-- This problem was asked by Twitter.
-- 
-- Given a binary tree, find the lowest common ancestor (LCA) of two given 
-- nodes in the tree. Assume that each node in the tree also has a pointer to 
-- its parent.
-- 
-- According to the definition of LCA on Wikipedia: “The lowest common 
-- ancestor is defined between two nodes v and w as the lowest node in T that 
-- has both v and w as descendants (where we allow a node to be a descendant of 
-- itself).”

-- Consider a tree represented by a set of pairs (u, v), where the presence of 
-- a pair indicates that there exists an edge in the graph incident at node 
-- u and destined to node v. 
--
-- Then construct a data structure as a list of lists where each sub list contains 
-- exactly one (u, v) pair from the graph. 
--
-- Then start with the pairs (u, v), (w, x) where v and x are the nodes in question
-- and search the list for pairs (_, u) and (_, w). Perform a union on the lists
-- containing the (u, v) and (_, u) pairs and on the lists containing the (w, x) and 
-- (_, w) pairs.

findParent :: (Eq a) => [(a, a)] -> [(a, a)] -> [(a, a)] -> a
findParent vSet uSet adjList = 
    if (fst . head) uSet == (fst . head) vSet
        then (fst . head) uSet
        else findParent uSet' vSet' adjList
    where
        nextTup iSet = [ (s, t) | (s, t) <- adjList, t == (fst . head) iSet ]
        nextU = nextTup uSet
        nextV = nextTup vSet
        -- Should check to see if uSet and vSet have reached
        -- fixed points under this operation so that this 
        -- method won't run forever if u and v don't actually share
        -- a common ancestor.
        uSet' = if null nextU then uSet else (head nextU) : uSet
        vSet' = if null nextV then vSet else (head nextV) : vSet

commonAncestor :: (Eq a) => a -> a -> [(a, a)] -> a
commonAncestor u v adjList = findParent uSet vSet adjList
    where
        firstTup i = [ (s, t) | (s, t) <- adjList, t == i ]
        uSet = firstTup u
        vSet = firstTup v

--   1
--  / \
-- 2   3
--    / \
--   4   5

test1 :: IO ()
test1 = do
    let adjList = [ (1, 2)
                  , (1, 3)
                  , (3, 4)
                  , (3, 5)
                  ]
        res = commonAncestor 2 3 adjList
    putStrLn $ show res

main :: IO ()
main = test1
