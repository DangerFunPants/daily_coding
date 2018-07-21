#!/usr/bin/env runhaskell

-- This problem was asked by Jane Street.
--
-- cons(a, b) constructs a pair, and car(pair) and cdr(pair) returns the first 
-- and last element of that pair. For example, car(cons(3, 4)) returns 3, 
-- and cdr(cons(3, 4)) returns 4.
--
-- Given this implementation of cons:
--
-- def cons(a, b):
--     def pair(f):
--         return f(a, b)
--     return pair
--
-- Implement car and cdr.

cons :: a -> b -> (a -> b -> c) -> c
cons a b = \f -> (f a b)

p1 :: ((a -> b -> a) -> a) -> a
p1 p = p (\a _ -> a)

p2 :: ((a -> b -> b) -> b) -> b
p2 p = p (\_ b -> b)

sumPair :: Num a => ((a -> a -> a) -> a) -> a
sumPair p = p (+)

main :: IO ()
main = do
    putStrLn $ "Hola Mundas!"
    putStrLn $ "Pair: "++(show p)
    putStrLn $ "Left projection: "++(show l)
    putStrLn $ "Right projection: "++(show r)
    putStrLn $ "Sum: "++(show sumP)
    where
        p = (1, 2)
        p' = cons 1 2
        l = p1 p'
        r = p2 p'
        sumP = sumPair p'