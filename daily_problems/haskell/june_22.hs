#!/usr/bin/env runhaskell

-- This problem was asked by Amazon.
--
-- There exists a staircase with N steps, and you can climb up either 1 or 2 
-- steps at a time. Given N, write a function that returns the number of unique 
-- ways you can climb the staircase. The order of the steps matters.
--
-- For example, if N is 4, then there are 5 unique ways:
--
--     1, 1, 1, 1
--     2, 1, 1
--     1, 2, 1
--     1, 1, 2
--     2, 2
--
-- What if, instead of being able to climb 1 or 2 steps at a time, you could 
-- climb any number from a set of positive integers X? For example, if X = {1, 
-- 3, 5}, you could climb 1, 3, or 5 steps at a time.

-- This is computing fibonacci numbers O(1.6^n)
countSteps :: Int -> Int
countSteps 0 = 1
countSteps 1 = 1
countSteps n = (countSteps $ n - 1) + (countSteps $ n - 2)

-- This computes paths for possible counts of { 1, 3, 5 } only 
countSteps' :: Int -> Int
countSteps' 0 = 1
countSteps' 1 = 1
countSteps' n
    | n < 0 = 0
    | otherwise = (countSteps' $ n - 1) + (countSteps' $ n - 3) + (countSteps' $ n - 5)

-- Computes counts for arbitrary step counts with no memoization, O(2^n)
countSteps'''' :: Int -> [Int] -> Int
countSteps'''' 0 _ = 1
countSteps'''' n set
    | n < 0 = 0
    | (minimum set) == n = 1
    | otherwise = foldl1 (+) (fmap (\i -> countSteps'''' (n-i) set) set)

-- Computes fibonacci numbers with memoization - O(n)
countSteps''' :: Integer -> (Integer, [(Integer, Integer)])
countSteps''' 0 = (1, [(0, 1)])
countSteps''' n = res
    where
        (recCallResult, memo) = (countSteps''' (n - 1))
        computed = recCallResult + (snd (head memo))
        newMemo = (((n - 1), recCallResult):memo)
        res = (computed, newMemo)

-- Computes counts for arbitrary step counts with memoization O(n)
-- assumes that the step count list is sorted. 
countSteps'' :: Int -> [Int] -> (Int, [(Int, Int)])
countSteps'' 0 _ = (1, [(0, 1)])
countSteps'' n set
    | n < 0 = (0, [])
    | n == 0 = (1, [(0, 1)])
    | otherwise = res
        where
            (recCallResult, memo) = (countSteps'' (n - (minimum set)) set)
            ms = fmap ((\m -> case m of Nothing -> 0; (Just x) -> x) . ((flip lookup) memo)) (tail (fmap (\i -> (n - i)) set))
            ms = 
            newMemo = ((n-1, recCallResult):memo)
            computed = recCallResult + (sum ms)
            res = (computed, newMemo)

main :: IO ()
main = do
    let ls = [0..100]
    let set = [1,3,5]
    let stepCount = fmap (fst . ((flip countSteps'') set)) ls
    putStrLn $ "Step count is: "++(show stepCount)