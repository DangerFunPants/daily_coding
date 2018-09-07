-- This problem was asked by LinkedIn.
-- 
-- Determine whether a tree is a valid binary search tree.
-- 
-- A binary search tree is a tree with two children, left and right, and 
-- satisfies the constraint that the key in the left child must be less than or 
-- equal to the root and the key in the right child must be greater than or 
-- equal to the root.

import Text.Show.Pretty

data BTree a 
    = Node a (BTree a) (BTree a)
    | Nil
    deriving (Show, Eq)

isValidBst (Node v lc rc) f = 
    if f v
        then isValidBst lc (<= v) && isValidBst rc (>= v)
        else False
isValidBst Nil _ = True

test1 :: IO ()
test1 = do
    let ts = Node 10
                (Node 5
                    (Node 3 Nil Nil)
                    (Node 6 Nil Nil))
                (Node 20
                    (Node 15 Nil Nil)
                    (Node 25 Nil Nil))
    let ts' = Node 10
                 (Node 5
                     (Node 3 Nil Nil)
                     (Node 6 Nil Nil))
                 (Node 20
                     (Node 30 Nil Nil)
                     (Node 25 Nil Nil))
        res = isValidBst ts (\_ -> True)	
        res' = isValidBst ts' (\_ -> True)
    putStrLn $ ppShow ts
    putStrLn $ show res
    putStrLn $ ppShow ts'
    putStrLn $ show res'

main :: IO ()
main = test1
