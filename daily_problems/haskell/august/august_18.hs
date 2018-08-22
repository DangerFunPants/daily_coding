-- This problem was asked by Facebook.
-- 
-- Given a list of integers, return the largest product that can be made by 
-- multiplying any three integers.
-- 
-- For example, if the list is [-10, -10, 5, 2], we should return 500, since 
-- that's -10 * -10 * 5.
-- 
-- You can assume the list has at least three integers.

import Data.List (sort)

maxProd :: (Num a, Ord a) => [a] -> a
maxProd s = if t1 > t2 then t1 else t2
    where
        sorted = sort s
        last3 = take 3 (reverse sorted)
        first2 = take 2 sorted
        t1 = foldl1 (*) last3
        t2 = foldl1 (*) (last sorted:first2)

test1 :: IO ()
test1 = do
    let ts = [ -10, -10, 5, 2 ]
        res = maxProd ts
    putStrLn $ show res

main :: IO ()
main = test1
