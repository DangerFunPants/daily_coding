-- This problem was asked by Microsoft.
-- 
-- Print the nodes in a binary tree level-wise. For example, the following 
-- should print 1, 2, 3, 4, 5.
-- 
--   1
--  / \
-- 2   3
--    / \
--   4   5

data BTree a
    = Node a (BTree a) (BTree a)
    | Nil
    deriving (Eq)

instance (Show a) => Show (BTree a) where
    show (Node a lc rc) = show a ++ show lc ++ show rc
    show Nil = ""

test1 :: IO ()
test1 = do
    let ts = Node 1
                (Node 2 Nil Nil)
                (Node 3
                    (Node 4 Nil Nil)
                    (Node 5 Nil Nil))
    putStrLn $ show ts

main :: IO ()
main = test1
