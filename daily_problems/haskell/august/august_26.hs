-- This problem was asked by Snapchat.
-- 
-- Given a list of possibly overlapping intervals, return a new list of 
-- intervals where all overlapping intervals have been merged.
-- 
-- The input list is not necessarily ordered in any way.
-- 
-- For example, given [(1, 3), (5, 8), (4, 10), (20, 25)], you should return 
-- [(1, 3), (4, 10), (20, 25)].

import Data.List (sortOn)

findOverlap' :: (Ord a) => [(a, a)] -> (a, a) -> [(a, a)]
findOverlap' [] t = [t]
findOverlap' ((s, t):xs) (s', t') = 
    if s < t'
        then findOverlap' xs (s', (max t t'))
        else (s', t'):(findOverlap' xs (s, t))

findOverlap :: (Ord a) => [(a, a)] -> [(a, a)]
findOverlap ts = findOverlap' (tail sorted) (head sorted)
    where
        sorted = sortOn fst ts

test1 = do
    let ts = [ (1, 3)
             , (5, 8)
             , (4, 10)
             , (20, 25)
             ]
        res = findOverlap ts
    putStrLn $ show res


main :: IO ()
main = test1
