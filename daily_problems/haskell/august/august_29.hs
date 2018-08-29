-- This problem was asked by Google.
-- 
-- Given the root of a binary tree, return a deepest node. For example, in the 
-- following tree, return d.
-- 
--     a
--    / \
--   b   c
--  /
-- d

data BTree a
    = Node a (BTree a) (BTree a)
    | Leaf a
    | None

deepestNode :: BTree a -> (a, Int)
deepestNode (Node v None rc) = (rightVal, (succ rightCount))
    where
        (rightVal, rightCount) = deepestNode rc
deepestNode (Node v lc None) = (leftVal, (succ leftCount))
    where
        (leftVal, leftCount) = deepestNode lc
deepestNode (Node v lc rc) = 
    if leftCount > rightCount
        then (leftVal, (succ leftCount))
        else (rightVal, (succ rightCount))
    where
        (leftVal, leftCount) = deepestNode lc
        (rightVal, rightCount) = deepestNode rc
deepestNode (Leaf v) = (v, 1)

test1 = 
    let ts = Node 'a' (Node 'b' (Node 'x' (Leaf 'z') None) (Leaf 'e')) (Node 'c' (Leaf 'f') (Leaf 'g'))
    in putStrLn $ show (deepestNode ts)

main = test1
