-- This problem was asked by Two Sigma.
-- 
-- Using a function rand7() that returns an integer from 1 to 7 (inclusive) 
-- with uniform probability, implement a function rand5() that returns an 
-- integer from 1 to 5 (inclusive).

import System.Random (getStdRandom, randomR)
import Control.Monad (mapM)

rand7 :: IO Int
rand7 = getStdRandom (randomR (1, 7))

rand5 :: IO Int
rand5 = do
    r <- rand7
    if r > 5
        then rand5
        else return r

test1 :: IO ()
test1 = do
    res <- mapM (\_ -> rand5) [1..1000000]
    let stat = fmap (\i -> (fromIntegral (length $ (filter (==i) res))) / (fromIntegral (length res))) [1..5]
    putStrLn $ show stat

main :: IO ()
main = test1
