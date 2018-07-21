
-- This problem was asked by Google.
-- 
-- Given an array of integers where every integer occurs three times except for 
-- one integer, which only occurs once, find and return the non-duplicated 
-- integer.
-- 
-- For example, given [6, 1, 3, 3, 3, 6, 6], return 1. Given [13, 19, 13, 13], 
-- return 19.
-- 
-- Do this in O(N) time and O(1) space.

import qualified Data.Map as M
import Data.List

countNums :: [Int] -> M.Map Int Int -> M.Map Int Int
countNums [] m = m
countNums (x:xs) m = countNums xs newDict
    where
        newDict = M.insertWith (+) x 1 m

nonDuplicates xs = (fst . head . (sortOn snd) . M.toList) (countNums xs M.empty)

test1 :: IO ()
test1 = do
    let ts = [ [6, 1, 3, 3, 3, 6, 6]
             , [13, 19, 13, 13]
             ]
        res = fmap nonDuplicates ts
    mapM (putStrLn . show) res
    return ()


main :: IO ()
main = do
    putStrLn $ "Hola Mundas!" 
