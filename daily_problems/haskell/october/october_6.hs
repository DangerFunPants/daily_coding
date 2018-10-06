-- This problem was asked by Google.
-- 
-- Given a sorted list of integers, square the elements and give the output in 
-- sorted order.
-- 
-- For example, given [-9, -2, 0, 2, 3], return [0, 4, 4, 9, 81].

import Data.List (sort)

squareSort :: (Ord a, Num a, Floating a) => [a] -> [a]
squareSort = (sort . (fmap (**2)))

test1 :: IO ()
test1 = do
    let ts = [ -9, -2, 0, 2, 3 ]
        res = squareSort ts
    putStrLn $ show res

main :: IO ()
main = test1
