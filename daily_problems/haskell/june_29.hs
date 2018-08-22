#!/usr/bin/env runhaskell

--- This problem was asked by Facebook.
---
--- A builder is looking to build a row of N houses that can be of K different 
--- colors. He has a goal of minimizing cost while ensuring that no two 
--- neighboring houses are of the same color.
---
--- Given an N by K matrix where the nth row and kth column represents the cost 
--- to build the nth house with kth color, return the minimum cost which achieves 
--- this goal

-- minCost :: [[Int]] -> Int -> Int -> [(Int, Int)] -> Int
minCost :: [[Int]] -> Int -> Int -> Int
minCost mat h c
    | h == (length mat) = 0
    | otherwise = minC
    where
        notSame = filter (\(lc, _) -> lc /= c) (zip [0..] (mat !! h))
        costs = fmap (\(nextC, cost) -> cost + (minCost mat (succ h) nextC)) notSame
        minC = minimum costs

test1 :: IO ()
test1 = do
    let mat = [[1, 3, 5], [2, 20, 20], [8, 4, 6]]
    let res = minCost mat (0) (-1)
    putStrLn $ "Result: "++(show res)  

main :: IO ()
main = do
    test1
    putStrLn $ "Hola Mundas!"