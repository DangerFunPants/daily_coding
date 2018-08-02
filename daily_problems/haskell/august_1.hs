-- This problem was asked by Google.
-- 
-- Implement an LRU (Least Recently Used) cache. It should be able to be 
-- initialized with a cache size n, and contain the following methods:
-- 
--     set(key, value): sets key to value. If there are already n items in the 
-- cache and we are adding a new item, then it should also remove the least 
-- recently used item.
--     get(key): gets the value at key. If no such key exists, return null.
-- 
-- Each operation should run in O(1) time.

import qualified Data.Map as M
import Control.Monad.State

type MapST k v a = State (M.Map k v) a

type LruST k v a = State ((M.Map k v), [k], Int) a

insertM :: (Ord k) => k -> v -> LruST k v ()
insertM k v = state $ \(m, q, n) -> let newMap = if (M.size m) >= n
                                                    then (M.insert k v (M.delete (last q) m))
                                                    else (M.insert k v m)
                                        newQ = if (M.size m) >= n
                                                then (k:(init q))
                                                else (k:q)
                                     in ((), (newMap, newQ, n))

-- This will require O(n) in the worst case (when the LRU is accessed) 
lookupM :: (Ord k) => k -> LruST k v (Maybe v)
lookupM k = state $ \(m, q, n) -> let res = M.lookup k m
                                      newQ = case res of 
                                                Nothing -> q
                                                (Just _) -> k:(remove k q) -- Need to get this down to O(1) not O(n)
                                   in (res, (m, newQ, n))

deleteM :: (Ord k) => k -> LruST k v ()
deleteM k = state $ \(m, q, n) -> let newMap = M.delete k m
                                   in ((), (newMap, q, n))

dictTest :: LruST Int String (Maybe String)
dictTest = do
    insertM 1 "Hello"
    insertM 2 "World"
    v <- lookupM 1
    insertM 3 "Test"
    insertM 4 "Test2"
    insertM 5 "Test3" 
    return v

-- qTest :: MapST Int String ()
-- qTest = do
--     let n = 2
--     q <- setQ 1 "Hello" [] n
--     q' <- setQ 2 "Hello2" q n
--     q'' <- setQ 3 "Hello3" q' n
--     q''' <- getQ 2 q'' n
--     return q'''
-- 
-- setQ :: (Ord k) => k -> v -> [k] -> Int -> MapST k v [k]
-- setQ k v que n = do
--     let newQ = (k:que)
--     if (length newQ) > n
--         then do
--             let to_rem = (last newQ)
--                 newQ' = (init newQ)
--             deleteM to_rem
--             insertM k v
--             return newQ'
--         else do
--             insertM k v
--             return newQ
-- 
-- getQ :: (Ord k) => k -> [k] -> Int -> MapST k v ((Maybe v), [k])
-- getQ k que n = do
--     let newQ = k:(remove k que)
--     v <- lookupM k
--     return (v, newQ)
-- 
remove :: (Eq a) => a -> [a] -> [a]
remove v (a:as) = if a == v then (remove v as) else (a:(remove v as))
remove _ [] = []
--     
-- 
test1 :: IO ()
test1 = do
    let s = (runState dictTest) (M.empty, [], 4)
    putStrLn $ show s


main :: IO ()
main = do
    putStrLn $ "Hola Mundas!"
