-- This problem was asked by Facebook.
-- 
-- Given three 32-bit integers x, y, and b, return x if b is 1 and y if b is 0, 
-- using only mathematical or bit operations. You can assume b can only be 1 or 
-- 0.

import Data.Bits

testIfSet :: Int -> Int -> Int -> Int
testIfSet x y b = (x .&. mult) .|. (y .&. (complement mult))
    where   
        mult = b * (complement 0)

test1 :: IO ()
test1 = do
    let res = testIfSet 25 50 0
    putStrLn $ show res

main :: IO ()
main = test1
