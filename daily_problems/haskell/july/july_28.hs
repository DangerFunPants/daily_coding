-- This problem was asked by Google.
-- 
-- Given pre-order and in-order traversals of a binary tree, write a function 
-- to reconstruct the tree.
-- 
-- For example, given the following preorder traversal:
-- 
-- [a, b, d, e, c, f, g]
-- 
-- And the following inorder traversal:
-- 
-- [d, b, e, a, f, c, g]
-- 
-- You should return the following tree:
-- 
--     a
--    / \
--   b   c
--  / \ / \
-- d  e f  g
--
-- Note that the tree needs to be "Full" Meaning that each node
-- has either 0 or 2 children. 

data BTree a 
    = Node a (BTree a) (BTree a)
    | Leaf a
    deriving (Show)

testTree = Node 'a' (Node 'b' (Leaf 'd') (Leaf 'e')) (Node 'c' (Leaf 'f') (Leaf 'g'))

inOrder (Node v lc rc) = (inOrder lc)++[v]++(inOrder rc)
inOrder (Leaf v) = [v]

preOrder (Node v lc rc) = [v]++(preOrder lc)++(preOrder rc)
preOrder (Leaf v) = [v]

smallTree = Node 'a' (Leaf 'b') (Leaf 'c')

leftHalf :: [a] -> [a]
leftHalf as = take (l `div` 2) as
    where
        l = length as

rightHalf :: [a] -> [a]
rightHalf as = reverse (take (l `div` 2) (reverse as))
    where
        l = length as

reconstruct (a:[]) ps = ((Leaf a), (tail ps))
reconstruct inO (p:ps) = (thisNode, ps'')
    where
        (lc, ps')  = reconstruct (leftHalf inO) ps
        (rc, ps'') = reconstruct (rightHalf inO) ps'
        thisNode = Node p lc rc

test1 :: IO ()
test1 = do
    let (io, po) = ((inOrder testTree), (preOrder testTree))
        res = reconstruct io po
    putStrLn $ show res

main :: IO ()
main = test1
