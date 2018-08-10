-- This problem was asked by Facebook.
-- 
-- Given a multiset of integers, return whether it can be partitioned into two 
-- subsets whose sums are the same.
-- 
-- For example, given the multiset {15, 5, 20, 10, 35, 15, 10}, it would return 
-- true, since we can split it up into {15, 5, 10, 15, 10} and {20, 35}, which 
-- both add up to 55.
-- 
-- Given the multiset {15, 5, 20, 10, 35}, it would return false, since we 
-- can't split it up into two subsets that add up to the same sum.

-- Generate all possible partitions and then check their sum. 

-- { 1, 2, 3, 4 }
-- { 1 } { 2, 3, 4 }
-- { 1, 2 } { 3, 4 }
-- { 1, 2, 3 } { 4 }
-- 

import Control.Monad (filterM)

powerset :: [a] -> [[a]]
powerset = filterM (\_ -> [ True, False ])

multiset :: (Num a, Eq a, Integral a) => [a] -> Bool
multiset s = if even setSum
                then not $ null (filter filterFn psets)
                else False
    where
        psets = powerset s
        setSum = sum s
        filterFn pset = (sum pset) == (setSum `div` 2)

test1 :: IO ()
test1 = do
    let ts = [ [ 15, 5, 20, 10, 35, 15, 10 ]
             , [ 15, 5, 20, 10, 35 ]
             ]
        bs = fmap multiset ts
    putStrLn $ show (zip ts bs)


main :: IO ()
main = do
    test1
