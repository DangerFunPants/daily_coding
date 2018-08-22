#!/usr/bin/env runhaskell

import qualified Data.Dequeue as D

-- Given an array of integers and a number k, where 1 <= k <= length of the 
-- array, compute the maximum values of each subarray of length k.
--
-- For example, given array = [10, 5, 2, 7, 8, 7] and k = 3, we should get: 
-- [10, 7, 8, 8], since:
--
-- 10 = max(10, 5, 2)
-- 7  = max(5, 2, 7)
-- 8  = max(2, 7, 8)
-- 8  = max(7, 8, 7)
--
-- Do this in O(n) time and O(k) space. You can modify the input array in-place 
-- and you do not need to store the results. You can simply print them out as 
-- you compute them.

-- O(K*N)
simple :: [Int] -> Int -> [Int]
simple ls k
    | (length ls >= k) = (maximum . take k) ls:(simple (tail ls) k)
    | otherwise = []

{-
Another example:
    [5, 10, 2, 7, 7, 8]
-}
-- linear :: D.Dequeue d => d q -> [Int] -> Int -> [Int]
-- linear q arr k = res
--     where
--         res = (takeBack 1 newQ):(linear newQ (tail arr) k)
--         newQ = 


linear' :: [(Int, Int)] -> Int -> Int -> [Int]
linear' [] _ _ = []
linear' arr k m = if ((snd . last) arr) > m
                    then ((fst . last) arr):(linear' (init arr) k ((snd . last) arr))
                    else (linear' (init arr) k m)

testSimple :: IO ()
testSimple = do
    let ls = [ 10, 5, 2, 7, 8, 7 ]
        k = 3
        res = simple ls k
    putStrLn $ show res

test1 :: IO ()
test1 = do
    let ls = [10, 5, 2, 7, 8, 7]
        k = 3
        i = linear' (zip [0..] (take k ls)) k (-1)
    putStrLn (show i)

main :: IO ()
main = do
    test1
    