#!/usr/bin/env runhaskell

-- This problem was asked by Facebook.
--
-- You are given an array of non-negative integers that represents a
-- two-dimensional elevation map where each element is unit-width wall and the
-- integer is the height. Suppose it will rain and all spots between two walls
-- get filled up.
--
-- Compute how many units of water remain trapped on the map in O(N) time and
-- O(1) space.
--
-- For example, given the input [2, 1, 2], we can hold 1 unit of water in the
-- middle.
--
-- Given the input [3, 0, 1, 3, 0, 5], we can hold 3 units in the first index,
-- 2 in the second, and 3 in the fourth index (we cannot hold 5 since it would
-- run off to the left), so we can trap 8 units of water.

countWater' :: [(Int, Int)] -> [Int] -> Int
countWater' ((l, r):rest) (w:ws) = h + (countWater' rest ws)
    where
        tallest = (min l r)
        h = max 0 (tallest - w)
countWater' [] [] = 0

countWater :: [Int] -> Int
countWater ls = countWater' (largest ls) ls

calculateLargest :: [Int] -> Int -> [Int]
calculateLargest (e:es) lg = lg:(calculateLargest es newLg)
    where
        newLg = if e > lg then e else lg
calculateLargest [] _ = []

leftLargest ls = calculateLargest ls 0
rightLargest ls = reverse (calculateLargest (reverse ls) 0)
largest ls = zip (leftLargest ls) (rightLargest ls)

ls1 :: [Int]
ls1 = [3,0,1,3,0,5]

ls2 :: [Int]
ls2 = [3,0,6,1,3,0,5]

test :: [Int] -> IO ()
test ls = do
    let res = countWater ls
    putStrLn $ "Result is: "++(show res)

main :: IO ()
main = do
    let tests = [ [ 3, 0, 1, 3, 0, 5 ]
                , [ 2, 1, 2 ]
                ]
    mapM test tests
    return ()
