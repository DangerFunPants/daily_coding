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

data BTree a 
    = Node a (BTree a) (BTree a)
    | Leaf a
    deriving (Show)


-- reconstruct :: String -> ((BTree Char), String)
-- reconstruct (e:es) = (thisNode, rem')
--     where
--         thisNode = Node e leftCall rightCall
--         (leftCall, rem) = reconstruct es
--         (rightCall, rem') = reconstruct rem
-- reconstruct [] = (Nil, [])

testTree = Node 'a' (Node 'b' (Leaf 'd') (Leaf 'e')) (Node 'c' (Leaf 'f') (Leaf 'g'))

inOrder (Node v lc rc) = (inOrder lc)++[v]++(inOrder rc)
inOrder (Leaf v) = [v]

preOrder (Node v lc rc) = [v]++(preOrder lc)++(preOrder rc)
preOrder (Leaf v) = [v]



-- test1 :: IO ()
-- test1 = do
--     let ts = ['a', 'b', 'd', 'e', 'f', 'g' ]
--         res = reconstruct ts
--     putStrLn $ show res

test1 = putStrLn "Hola" 
main :: IO ()
main = test1
