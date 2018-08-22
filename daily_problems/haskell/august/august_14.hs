-- This problem was asked by Square.
-- 
-- Assume you have access to a function toss_biased() which returns 0 or 1 with 
-- a probability that's not 50-50 (but also not 0-100 or 100-0). You do not 
-- know the bias of the coin.
-- 
-- Write a function to simulate an unbiased coin toss.

import System.Random
import Control.Monad (mapM)

tossBiased :: IO Bool
tossBiased = do
    r <- getStdRandom (randomR (1,10000)) :: (IO Int)
    if r > 1000
        then return False
        else return True

tossFair :: IO Bool
tossFair = do
    t1 <- tossBiased
    t2 <- tossBiased
    if t1 /= t2
        then return t1
        else tossFair

test1 = do
    rvs <- mapM (\_ -> tossFair) [1..100000]
    let ts = filter id rvs
        fs = filter not rvs
        cs = (length ts, length fs)
    putStrLn $ show cs

main :: IO ()
main = test1
    

