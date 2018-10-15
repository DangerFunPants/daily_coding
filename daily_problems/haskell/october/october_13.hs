-- This problem was asked by Google.
-- 
-- Given the root of a binary search tree, and a target K, return two nodes in 
-- the tree whose sum equals K.
-- 
-- For example, given the following tree and K of 20
-- 
--     10
--    /   \
--  5      15
--        /  \
--      11    15
-- 
-- Return the nodes 5 and 15.

data BTree a
    = Node a (BTree a) (BTree a)
    | Nil

inOrder :: (BTree a) -> [a]
inOrder (Node a lc rc) = (inOrder lc) ++ [a] ++ (inOrder rc)
inOrder Nil = []

findSumList :: (Num a, Ord a) => [a] -> a -> Maybe (a, a)
findSumList (a:[]) _ = Nothing
findSumList as k =
    if (first + final) > k
        then findSumList (init as) k
        else if (first + final) == k
                then return (first, final)
                else findSumList (tail as) k
    where 
        first = (head as)
        final = (last as)

findSum :: (Num a, Ord a) => (BTree a) -> a -> Maybe (a, a)
findSum bt k = findSumList sorted k
    where
        sorted = inOrder bt

test2 :: IO ()
test2 = do
    let ts = Node 10
                (Node 5 Nil Nil)
                (Node 16
                    (Node 11 Nil Nil)
                    (Node 17 Nil Nil))
        res = findSum ts 20
    putStrLn $ show res

test1 :: IO ()
test1 = do
    let ts = Node 10
                (Node 5 Nil Nil)
                (Node 15
                    (Node 11 Nil Nil)
                    (Node 15 Nil Nil))
        res = findSum ts 20
    putStrLn $ show res

main :: IO ()
main = test1 >> test2
