import Control.Lens

bellmanFord' :: Int -> [Int] -> [Int] -> [(Int, Int, Int)] -> ([Int], [Int])
bellmanFord' 0 dist preds _ = (dist, preds)
bellmanFord' c dist preds es = bellmanFord' (pred c) newDist newPred es
    where
        newDist = foldl (\ds (u, v, w) -> let newV = (dist !! (pred u)) + w
                                              oldV = (dist !! (pred v))
                                          in if newV < oldV
                                                then (element (pred v) .~ newV) ds
                                                else ds) dist es
        newPred = foldl (\ps (u, v, w) -> let newV = (dist !! (pred u)) + w
                                              oldV = (dist !! (pred v))
                                          in if newV < oldV
                                                 then (element (pred v) .~ u) ps
                                                 else ps) preds es

bellmanFord :: [(Int, Int, Int)] -> [Int] -> Int -> ([Int], [Int])
bellmanFord es vs s = bellmanFord' (succ vertC) dist (replicate vertC 0) es
    where
        vertC = length vs
        dist = [ if n == s then 0 else 1000000000 | n<-[1..vertC] ]

addEdge :: (Int, Int, Int) -> [(Int, Int, Int)] -> [(Int, Int, Int)]
addEdge e@(u, v, w) es = e:((v, u, w):es)

testBF :: IO ()
testBF = do
    let ts = [ (1, 2, 1)
             , (1, 3, 1)
             , (2, 4, 1)
             , (2, 3, 1)
             , (3, 5, 1)
             , (4, 5, 1)
             , (4, 6, 1)
             ]
        es = foldl (\l t -> addEdge t l) [] ts
        res = bellmanFord ts [1..6] 1
    putStrLn $ show es
    putStrLn $ show res

main :: IO ()
main = testBF
