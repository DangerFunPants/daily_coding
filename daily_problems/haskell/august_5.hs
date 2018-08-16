-- This problem was asked by Google.
-- 
-- Given an undirected graph represented as an adjacency matrix and an integer 
-- k, write a function to determine whether each vertex in the graph can be 
-- colored such that no two adjacent vertices share the same color using at 
-- most k colors.

badColor (n:ns) k = concat ret
    where
        thisNode = fmap (\c -> (n, c)) [1..k]
        rec = badColor ns k
        ret = fmap (\ls -> fmap (\p -> p:ls) thisNode) rec
badColor [] k = [[]]

testColor :: [(Int, Int)] -> [[Int]] -> Bool
testColor [] _ = True
testColor ((node, color):cs) adjMat =
    if null conflict
        then testColor cs adjMat
        else False
    where
        adj = adjMat !! node
        conflict = filter (\n -> (n,color) `elem` cs) adj

color adjMat k = (not . null . (filter id)) bs
    where
        ps = badColor [0..((pred . length) adjMat)] k
        bs = fmap (\p -> testColor p adjMat) ps
        
test1 :: IO ()
test1 = do
    let ts = [ [ 1 ] 
             , [ 0, 2, 3, 4 ]
             , [ 1, 4 ]
             , [ 1 ]
             , [ 1, 2 ]
             ]
        res = color ts 3
    putStrLn $ "Should be True: " ++ show res

test2 :: IO ()
test2 = do
    let adjMat = [ [ 5, 1, 4 ]
                 , [ 0, 2, 6 ]
                 , [ 1, 3, 7 ]
                 , [ 2, 4, 8 ]
                 , [ 0, 3, 9 ]
                 , [ 0, 8, 7 ]
                 , [ 1, 8, 9 ]
                 , [ 2, 5, 9 ]
                 , [ 3, 6, 5 ]
                 , [ 4, 6, 7 ]
                 ]
        res1 = color adjMat 2
        res2 = color adjMat 3
    putStrLn $ "Peterson graph with 2: " ++ show res1
    putStrLn $ "Peterson graph with 3: " ++ show res2


main :: IO ()
main = do
    test1
    test2
