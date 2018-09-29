-- This problem was asked by Facebook.
-- 
-- Given a binary tree, return all paths from the root to leaves.
-- 
-- For example, given the tree
-- 
--    1
--   / \
--  2   3
--     / \
--    4   5
-- 
-- it should return [[1, 2], [1, 3, 4], [1, 3, 5]].

leafPaths :: BTree a -> [[a]]
leafPaths (Node v Nil Nil) = [[v]]
leafPaths (Node v lc rc) = (fmap ([v]++) leftPaths) ++ (fmap ([v]++) rightPaths)
    where
        rightPaths = leafPaths rc
        leftPaths = leafPaths lc

data BTree a
    = Nil
    | Node a (BTree a) (BTree a)
    deriving (Show, Eq)

test1 :: IO ()
test1 = do
    let ts = Node 1 
                (Node 2 Nil Nil) 
                (Node 3 
                    (Node 4 Nil Nil)
                    (Node 5 Nil Nil))
        res = leafPaths ts
    putStrLn $ show ts
    putStrLn $ show res

main :: IO ()
main = test1
