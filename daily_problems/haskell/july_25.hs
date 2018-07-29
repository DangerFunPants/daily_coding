-- This problem was asked by Two Sigma.
-- 
-- Using a function rand5() that returns an integer from 1 to 5 (inclusive) 
-- with uniform probability, implement a function rand7() that returns an 
-- integer from 1 to 7 (inclusive).

import System.Random
import Control.Monad (mapM)

rand5 :: IO Int
rand5 = getStdRandom (randomR (1,5))

-- If the die given gave values over a range that was a multiple
-- of 7 then you could reduce modulo 7 and get a fair die: 
-- Ex. Consider a 14 sided die.
-- 0 -> 0
-- 1 -> 1
--  .
--  .
-- 7 -> 7
-- 8 -> 1
-- 9 -> 2
-- 10 -> 3
-- 11 -> 4
-- 12 -> 5
-- 13 -> 6
-- Since there are exactly two ways to roll each number, assuming that
-- the dice is fair, the reduction modulo 7 yields a uniform distributin 
-- in the range 1 - 7. The same principle applies for any multiple of 7. 
--
-- This generalizes the problem to generating a random number that is a multiple
-- of 7 using a series of random numbers ranging from 1 .. 5
--
--
rand7 :: IO Int
rand7 = do



main :: IO ()
main = do
    putStrLn $ "Hola Mundas" 
    r <- rand5
    putStrLn $ (show r)
