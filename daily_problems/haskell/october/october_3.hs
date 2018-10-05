-- This problem was asked by Google.
-- 
-- Given two non-empty binary trees s and t, check whether tree t has exactly 
-- the same structure and node values with a subtree of s. A subtree of s is a 
-- tree consists of a node in s and all of this node's descendants. The tree s 
-- could also be considered as a subtree of itself.

import Text.Show.Pretty (ppShow)

data BTree a
    = Node a (BTree a) (BTree a)
    | Nil
    deriving (Show, Eq)

isSubTree :: (Eq a) => (BTree a) -> (BTree a) -> Bool
isSubTree t@(Node a lc rc) child = 
    if t == child
        then True
        else (isSubTree lc child) || (isSubTree rc child)
isSubTree Nil _ = False

test1 :: IO ()
test1 = do
    let ts = Node 1
                (Node 2
                    (Node 3 Nil Nil)
                    (Node 4 Nil Nil))
                (Node 5
                    (Node 6 Nil Nil)
                    (Node 7 Nil Nil))
        sub = Node 2 
                (Node 3 Nil Nil)
                (Node 4 Nil Nil)
        notSub = Node 2
                    (Node 10 Nil Nil)
                    (Node 7 Nil Nil)
        res = isSubTree ts sub
        non = isSubTree ts notSub
        
    putStrLn $ ppShow ts
    putStrLn $ ppShow res
    putStrLn $ ppShow non

main :: IO ()
main = test1
