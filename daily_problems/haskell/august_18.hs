-- This problem was asked by Facebook.
-- 
-- Given a list of integers, return the largest product that can be made by 
-- multiplying any three integers.
-- 
-- For example, if the list is [-10, -10, 5, 2], we should return 500, since 
-- that's -10 * -10 * 5.
-- 
-- You can assume the list has at least three integers.

import Control.Monad (filterM)

pset :: [a] -> [[a]]
pset = filterM (\_ -> [True, False])

maxProd :: (Num a, Ord a) => [a] -> a
maxProd s = maximum $ fmap (foldl1 (*)) [ s' | s' <- (pset s), (length s') == 3 ]

test1 :: IO ()
test1 = do
    let ts = [ -10, -10, 5, 2 ]
        res = maxProd ts
    putStrLn $ show res

main :: IO ()
main = test1
