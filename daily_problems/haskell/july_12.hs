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

import qualified Data.Map as M

--          CAD, USD, EUR
rates = [ [ 1.0, 0.5, 0.5 ] -- CAD
        , [ 2.0, 1.0, 0.25 ] -- USD
        , [ 2.0, 4.0, 1.0 ] -- EUR
        ]


bellmanFord :: M.Map Int Int -> M.Map Int Int -> [(Int, Int, Int)] -> ((M.Map Int Int), (M.Map Int Int)) 
bellmanFord dist pred ((u,v,w):edges)  = (bellmanFord nd np edges)
    where
        du = case (M.lookup u dist) of
                    Nothing -> 1000000
                    Just v -> v
        dv = case (M.lookup v dist) of 
                    Nothing -> 1000000
                    Just v -> v
        np = case (du + w) < v of
                True -> M.insert v u pred
                False -> pred
        nd = case (du + w) < v of
                True -> M.insert v (du + w) dist
                False -> dist
bellmanFord dist pred [] = (dist, pred)

bfSearch :: [Int] -> [(Int, Int, Int)] -> Int -> ((M.Map Int Int), (M.Map Int Int))
bfSearch vs es src = bellmanFord (M.fromList [(src, 0)]) (M.fromList []) es

type a BFState = State ((Map Int Int), (Map Int Int)) a

getDist :: Int -> BFState (Maybe Int)
getDist v = state $ \s -> ((lookup v (fst s)), s)

getPred :: Int -> BFState (Maybe Int)
getPred v = state $ \s -> ((lookup v (snd s)), s)

insDist :: Int -> Int -> BFState ()
insDist k v = state $ \s -> let newDist = insert k v (fst s)
                                in ((), (newDist, (snd s))

insPred :: Int -> Int -> BFState ()
insPred k v = state $ \s -> let newPred = insert k v (snd s)
                                in ((), ((fst s), newPred))

bellmanFord' :: [(Int, Int, Int)] -> BFState ((Map Int Int), (Map Int Int))
bellmanFord' ((u, v, w):edges) = do
    du <- getDist u
    dv <- getDist v
    if (du + w) < dv
        then do
            insDist v (du + w)
            insPred v u
        else return ()
    bellmanFord' edges
bellmanFord' [] = do
    p <- getPred
    d <- getDist
    return (d, p)



















