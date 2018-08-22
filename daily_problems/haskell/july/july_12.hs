#!/usr/bin/env runhaskell

-- This problem was asked by Jane Street.
-- 
-- Suppose you are given a table of currency exchange rates, represented as a 
-- 2D array. Determine whether there is a possible arbitrage: that is, whether 
-- there is some sequence of trades you can make, starting with some amount A 
-- of any currency, so that you can end up with some amount greater than A of 
-- that currency.
-- 
-- There are no transaction costs and you can trade fractional quantities.


import Control.Lens
import qualified Data.Map as M

bellmanFord' :: (Num a, Ord a)
             => Int 
             -> M.Map Int a 
             -> M.Map Int Int
             -> [(Int, Int, a)] 
             -> Maybe (M.Map Int a, M.Map Int Int)
bellmanFord' 0 dist preds es = do
    validDist <- checkWeight es dist
    return (validDist, preds)
bellmanFord' c dist preds es = do
    (newDist, newPreds) <- updateDist dist preds es
    bellmanFord' (pred c) newDist newPreds es

updateDist :: (Num a, Ord a)
           => M.Map Int a
           -> M.Map Int Int
           -> [(Int, Int, a)]
           -> Maybe (M.Map Int a, M.Map Int Int)
updateDist dist preds [] = return (dist, preds)
updateDist dist preds ((u, v, w):es) = do
    du <- M.lookup u dist
    dv <- M.lookup v dist
    if (du + w) < dv
        then do 
            let newPreds = M.insert v u preds
                newDist = M.insert v (du + w) dist
            updateDist newDist newPreds es
        else updateDist dist preds es

bellmanFord :: (Num a, Ord a)
            => [(Int, Int, a)] 
            -> [Int] 
            -> Int 
            -> Maybe (M.Map Int a, M.Map Int Int)
bellmanFord es vs s = bellmanFord' vertC dist pred es
    where
        vertC = length vs
        dist = M.fromList $ zip [1..vertC] [ if n == s then 0 else 1000000000 | n<-[1..vertC] ]
        pred = M.fromList $ zip [1..vertC] [0..]

checkWeight :: (Num a, Ord a)
            => [(Int, Int, a)] -> M.Map Int a -> Maybe (M.Map Int a)
checkWeight [] dist = return dist
checkWeight ((u, v, w):es) dist = do
    du <- M.lookup u dist
    dv <- M.lookup v dist
    if (du + w) < dv
        then Nothing
        else checkWeight es dist

addEdge :: (Num a, Ord a) 
        => (Int, Int, a) -> [(Int, Int, a)] -> [(Int, Int, a)]
addEdge e@(u, v, w) es = e:((v, u, w):es)

testBF :: IO ()
testBF = do
    let ts = [ (1, 2, 1)
             , (1, 3, -3)
             , (2, 4, 1)
             , (2, 3, 1)
             , (3, 5, 1)
             , (4, 5, 1)
             , (4, 6, 1)
             ]
        es = foldl (\l t -> addEdge t l) [] ts
        res = bellmanFord es [1..6] 1
    putStrLn $ show es
    putStrLn $ show res

testArbitrage :: IO ()
testArbitrage = do
    let ts = [ (1, 2, 0.33)
             , (2, 3, 277.5)
             , (3, 1, 0.0115)
             ]
        es = fmap (\(u, v, w) -> (u,v,(-(log w / log 10)))) ts
        res = bellmanFord es [1..3] 1
    putStrLn $ show res

test1 :: IO ()
test1 = do
    let ts = [ (1, 2, 2)
             , (2, 1, -3)
             ]
        res = bellmanFord ts [1..2] 1
    putStrLn $ show res

main :: IO ()
main = testArbitrage
