-- This problem was asked by Microsoft.
-- 
-- A number is considered perfect if its digits sum up to exactly 10.
-- 
-- Given a positive integer n, return the n-th perfect number.
-- 
-- For example, given 1, you should return 19. Given 2, you should return 28.

import Data.Char (digitToInt)

perfect :: [Integer] 
perfect = [ n | n<-[1..], (sum (fmap digitToInt (show n))) == 10 ]

test1 :: IO ()
test1 = do
    let ts = [0..10]
        res = fmap (\i -> perfect !! i) ts
    putStrLn $ show res

main :: IO ()
main = test1
