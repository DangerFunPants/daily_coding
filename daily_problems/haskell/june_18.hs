#!/usr/bin/env runhaskell

-- This problem was asked by Google.
--
-- A unival tree (which stands for "universal value") is a tree where all nodes
-- under it have the same value.
--
-- Given the root to a binary tree, count the number of unival subtrees.
--
-- For example, the following tree has 5 unival subtrees:
--
--    0
--   / \
--  1   0
--     / \
--    1   0
--   / \
--  1   1

data Node
    = Node Int Node Node
    | Leaf Int

countSubtrees :: Node -> (Int, Int)
countSubtrees (Leaf v) = (v, 1)
countSubtrees (Node v lc rc) = (v, newCount)
    where
        (ltVal, leftCount) = countSubtrees lc
        (rtVal, rightCount) = countSubtrees rc 
        newCount = if ltVal == v && rtVal == v 
                    then leftCount + rightCount + 1
                    else leftCount + rightCount

main :: IO ()
main = do
    let tr = Node 0 (Leaf 1) (Node 0 (Node 1 (Leaf 1) (Leaf 1)) (Leaf 0))
        res = countSubtrees tr
    putStrLn $ "Result is "++((show . snd) res)