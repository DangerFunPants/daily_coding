-- This problem was asked by Stripe.
-- 
-- Write a map implementation with a get function that lets you retrieve the 
-- value of a key at a particular time.
-- 
-- It should contain the following methods:
-- 
--     set(key, value, time): sets key to value for t = time.
--     get(key, time): gets the key at t = time.
-- 
-- The map should work like this. If we set a key at a particular time, it will 
-- maintain that value forever or until it gets set at a later time. In other 
-- words, when we get a key at a time, it should return the value that was set 
-- for that key set at the most recent time.
-- 
-- Consider the following examples:
-- 
-- d.set(1, 1, 0) # set key 1 to value 1 at time 0
-- d.set(1, 2, 2) # set key 1 to value 2 at time 2
-- d.get(1, 1) # get key 1 at time 1 should be 1
-- d.get(1, 3) # get key 1 at time 3 should be 2
-- 
-- d.set(1, 1, 5) # set key 1 to value 1 at time 5
-- d.get(1, 0) # get key 1 at time 0 should be null
-- d.get(1, 10) # get key 1 at time 10 should be 1
-- 
-- d.set(1, 1, 0) # set key 1 to value 1 at time 0
-- d.set(1, 2, 0) # set key 1 to value 2 at time 0
-- d.get(1, 0) # get key 1 at time 0 should be 2

import Text.Show.Pretty
import Data.List (sortOn)

type MT k v t = [(k, t, v)]

set :: (Eq k, Ord t) => k -> v -> t -> MT k v t -> MT k v t
set k v t d = (k, t, v):(filter (\(k', t', v') -> k/=k' || t/=t') d)

get :: (Eq k, Ord t, Num t) => k -> t -> MT k v t -> Maybe v
get k t d = 
    case null ks of 
        True -> Nothing
        False -> return $ ((fst . head) . (sortOn snd)) ks
    where
        ks = [ (v, t - t') | (k', t', v) <- d, k == k', t' <= t ]

test1 :: IO ()
test1 = do  
    let ts = ((set 1 2 2) . (set 1 1 0)) []
        res = (get 1 1 ts, get 1 3 ts)
    putStrLn $ ppShow res

test2 :: IO ()
test2 = do
    let ts = (set 1 1 5 [])
        res = (get 1 0 ts, get 1 10 ts)
    putStrLn $ ppShow res

test3 :: IO ()
test3 = do
    let ts = ((set 1 2 0) . (set 1 1 0)) []
        res = get 1 0 ts
    putStrLn $ ppShow res
 
main :: IO ()
main = test1 >> test2 >> test3
