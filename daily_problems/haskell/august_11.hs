-- This problem was asked by Facebook.
-- 
-- There is an N by M matrix of zeroes. Given N and M, write a function to 
-- count the number of ways of starting at the top-left corner and getting to 
-- the bottom-right corner. You can only move right or down.
-- 
-- For example, given a 2 by 2 matrix, you should return 2, since there are two 
-- ways to get to the bottom-right:
-- 
--     Right, then down
--     Down, then right
-- 
-- Given a 5 by 5 matrix, there are 70 ways to get to the bottom-right.

countWays :: Int -> Int -> Int
countWays 1 1 = 1
countWays 1 m = countWays 1 (pred m)
countWays n 1 = countWays (pred n) 1
countWays n m = (countWays (pred n) m) + (countWays n (pred m))

test1 :: IO ()
test1 = do
    let ts = [ (2, 2), (5, 5) ]
        res = fmap (uncurry countWays) ts
    putStrLn $ show (zip ts res)

main :: IO ()
main = do
    test1
