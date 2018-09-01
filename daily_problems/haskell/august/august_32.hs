-- This problem was asked by Google.
-- 
-- Invert a binary tree.
-- 
-- For example, given the following tree:
-- 
--     a
--    / \
--   b   c
--  / \  /
-- d   e f
-- 
-- should become:
-- 
--   a
--  / \
--  c  b
--  \  / \
--   f e  d

import Text.Show.Pretty

data BTree a 
    = Node a (BTree a) (BTree a) 
    | None
    deriving (Show, Eq)

invertTree :: BTree a -> BTree a
invertTree (Node a lc rc) = Node a right left
    where
        left = invertTree lc
        right = invertTree rc
invertTree None = None

test1 = do
    let ts = Node 'a' 
                (Node 'b' 
                    (Node 'd' None None) (Node 'e' None None)) 
                (Node 'c' 
                    (Node 'f' None None) None)
        res = invertTree ts
    putStrLn $ ppShow ts
    putStrLn $ ppShow res

