#!/usr/bin/env runhaskell

-- This problem was asked by Microsoft.
-- 
-- Compute the running median of a sequence of numbers. That is, given a stream 
-- of numbers, print out the median of the list so far on each new element.
-- 
-- Recall that the median of an even-numbered list is the average of the two 
-- middle numbers.
-- 
-- For example, given the sequence [2, 1, 5, 7, 2, 0, 5], your algorithm should 
-- print out:
-- 
-- 2
-- 1.5
-- 2
-- 3.5
-- 2
-- 2
-- 2

median :: [Int] -> Float
median ls = med
    where
        ll = (length ls)
        middle = case (odd ll) of 
                    True -> [ ls !! midPoint ]
                    False -> [ (ls !! midPoint), (ls !! (midPoint - 1)) ]
        med = (fromIntegral (sum middle)) / (fromIntegral (length middle))
        midPoint = ll `div` 2

calcMeds :: [Int] -> [Float]
calcMeds = (fmap median) . ((flip subs) [])

insertSorted :: [Int] -> Int -> [Int]
insertSorted ls v = (takeWhile (< v) ls)++[v]++(reverse (takeWhile (>=v) (reverse ls)))

subs :: [Int] -> [Int] -> [[Int]]
subs (a:as) prev = (s):(subs as s)
    where
        s = insertSorted prev a
subs [] _ = []

ts :: [Int]
ts = [2, 1, 5, 7, 2, 0, 5]

test1 :: IO ()
test1 = do
    let ts = [2, 1, 5, 7, 2, 0, 5]
        res = calcMeds ts
    putStrLn $ "Res for "++(show ts)++" : "++(show res)

main :: IO ()
main = do
    test1
