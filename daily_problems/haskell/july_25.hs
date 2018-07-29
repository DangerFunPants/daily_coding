-- This problem was asked by Two Sigma.
-- 
-- Using a function rand5() that returns an integer from 1 to 5 (inclusive) 
-- with uniform probability, implement a function rand7() that returns an 
-- integer from 1 to 7 (inclusive).

import System.Random
import Control.Monad (mapM)
import Data.List.Unique

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

-- Apparently this actually isn't doable without the possibility of infinite 
-- runtime ... 
--
-- Runtime
--  P{r > 20} = 4 / 25
--  P{Terminating on the Nth iteration} = 1 - (4 / 25)^N
--  lim_{n->inf} (1 - (4 / 25)^N) = 1
--  Thus P{Terminating on the Nth iteration} tends to 1 as N tends to infinity.
--  
--  For the sequence P_0 ... P_N:
--      P_N / P_{N-1} = 4/25
--  So the values of the sequence are decreasing exponentially. 

indexOf :: (Eq a) => a -> [a] -> Int
indexOf v (a:as) = if v == a
                        then 0
                        else 1 + (indexOf v as)
rand7 :: IO Int
rand7 = do
    r1 <- rand5
    r2 <- rand5
    let res = indexOf (r1, r2) [(a,b) | a<-[1..5],b<-[1..5]]

    if res > 20
        then rand7
        else return $ (res `mod` 7) + 1

mkHistogram' as es = fmap (\e -> length (filter (== e) as)) es

mkHistogram :: (Eq a, Ord a) => [a] -> [Int]
mkHistogram as = mkHistogram' as (sortUniq as)

main :: IO ()
main = do
    putStrLn $ "Hola Mundas" 
    r <- rand5
    putStrLn $ (show r)
