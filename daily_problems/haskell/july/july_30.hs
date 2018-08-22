-- This problem was asked by Microsoft.
-- 
-- Suppose an arithmetic expression is given as a binary tree. Each leaf is an 
-- integer and each internal node is one of '+', '−', '∗', or '/'.
-- 
-- Given the root to such a tree, write a function to evaluate it.
-- 
-- For example, given the following tree:
-- 
--     *
--    / \
--   +    +
--  / \  / \
-- 3  2  4  5
-- 
-- You should return 45, as it is (3 + 2) * (4 + 5).

data BTree a 
    = Node a (BTree a) (BTree a) 
    | Leaf a
    deriving (Show, Eq)

data Arith a
    = NumA a
    | BinOp (a -> a -> a)
    deriving (Show, Eq)

evalTree :: (Num a, Fractional a) => (BTree (Arith a)) -> a
evalTree (Node (BinOp op) lc rc) = res
    where
        (l, r) = ((evalTree lc), (evalTree rc))
        res = l `op` r
evalTree (Leaf (NumA v)) = v

test1 :: IO ()
test1 = do
    let ts = Node (BinOp (+))
                (Node (BinOp (+)) 
                    (Leaf (NumA 3.0)) 
                    (Leaf (NumA 2.0)))
                (Node (BinOp (+)) 
                    (Leaf (NumA 4.0)) 
                    (Leaf (NumA 5.0)))
        res = evalTree ts
    putStrLn $ show res

test2 = do
    let ts = Node (BinOp (*)) 
                (Node (BinOp (+)) 
                    (Leaf (NumA 3.0)) 
                    (Leaf (NumA 2.0))) 
                (Node (BinOp (+)) 
                    (Leaf (NumA 4.0)) 
                    (Leaf (NumA 5.0)))
        res = evalTree ts
    putStrLn $ show res

main :: IO ()
main = do
    test1
    test2
