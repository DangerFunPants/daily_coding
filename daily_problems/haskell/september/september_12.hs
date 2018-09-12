-- This problem was asked by Google.
-- 
-- Given a binary tree of integers, find the maximum path sum between two 
-- nodes. The path must go through at least one node, and does not need to go 
-- through the root.

import Text.Show.Pretty (ppShow)

data BTree a
    = Node a (BTree a) (BTree a)
    | Nil
    deriving (Show, Eq)

maxSum :: (Num a, Ord a) => BTree a -> a -> a
maxSum (Node v lc rc) c = maximum possibilities
    where
        possibilities = [ c
                        , maxSum lc (v + c)
                        , maxSum rc (v + c)
                        , maxSum lc v
                        , maxSum rc v
                        , maxSum lc 0 
                        , maxSum rc 0
                        ]
maxSum Nil c = c

test1 :: IO ()
test1 = do
    let ts = Node 1
                (Node (-10)
                    (Node 4 Nil Nil)
                    (Node 15 Nil Nil))
                (Node 3
                    (Node 1 Nil Nil)
                    (Node 1 Nil Nil))
        res = maxSum ts 0
    putStrLn $ ppShow ts
    putStrLn $ show res

main :: IO ()
main = test1
