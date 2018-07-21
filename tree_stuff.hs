#!/usr/bin/env runhaskell

data BTree a
    = Node a BTree BTree
    | Leaf a
    deriving (Show)

-- All of these basic Tree traversal methods run in O(N) time. 
countNodes :: BTree -> Int
countNodes (Leaf _) = 1
countNodes (Node _ l r) = succ $ (countNodes l) + (countNodes r)

maxDepth :: BTree -> Int
maxDepth (Leaf _) = 1
maxDepth (Node _ l r) = succ res
    where
        lc = maxDepth l
        rc = maxDepth r
        res = if (lc > rc) then lc else rc

deepestNode :: BTree -> (Int, BTree) 
deepestNode leaf@(Leaf _) = (1, leaf)
deepestNode (Node _ l r) = res
        where
            (lc, ln) = deepestNode l
            (rc, rn) = deepestNode r
            res = if (lc > rc) then ((succ lc), ln) else ((succ rc), rn)

main :: IO ()
main = do
    putStrLn $ "Result is: "++(show res)
    putStrLn $ "Max Depth is: "++(show maxD)
    putStrLn $ "Deepest node "++(show n)++" at depth "++(show maxD')
    where
        tr = Node 0 (Node 1 (Node 3 (Leaf 5) (Leaf 6)) (Leaf 4)) (Node 2 (Leaf 7) (Leaf 8))
        res = countNodes tr
        maxD = maxDepth tr
        (maxD', n) = deepestNode tr

