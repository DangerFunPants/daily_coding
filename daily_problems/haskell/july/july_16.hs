-- This problem was asked by Dropbox.
-- 
-- Given the root to a binary search tree, find the second largest node in the 
-- tree.

-- We want to find the parent of the maximum element...I think

data BST a 
    = Node a (BST a) (BST a) 
    | Leaf a

-- Easy way is to traverse the tree and then take the second largest element from the
-- list of nodes
inOrder :: (BST a) -> [a]
inOrder (Node v lc rc) = (inOrder lc)++[v]++(inOrder rc)
inOrder (Leaf v) = [v]

find2ndLargest :: Eq a => (BST a) -> a
find2ndLargest tr@(Node v lc rc) = res
    where
        traversal = inOrder tr
        maxs = takeWhile (\v -> (last traversal) == v) (reverse traversal)
        res = (reverse traversal) !! (length maxs)

test1 :: IO ()
test1 = do
    let tr = Node 1 (Leaf 0) (Node 5 (Node 1 (Leaf 0) (Leaf 2)) (Node 6 (Leaf 3) (Leaf 7)))
        res = find2ndLargest tr
    putStrLn $ show res

test2 :: IO ()
test2 = do
    let tr = Node 5 (Leaf 2) (Leaf 5)
        res = find2ndLargest tr
    putStrLn $ show res

main = do
    test1
    test2
