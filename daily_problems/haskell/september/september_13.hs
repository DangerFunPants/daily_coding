-- This problem was asked by Palantir.
-- 
-- Given a number represented by a list of digits, find the next greater 
-- permutation of a number, in terms of lexicographic ordering. If there is not 
-- greater permutation possible, return the permutation with the lowest 
-- value/ordering.
-- 
-- For example, the list [1,2,3] should return [1,3,2]. The list [1,3,2] should 
-- return [2,1,3]. The list [3,2,1] should return [1,2,3].
-- 
-- Can you perform the operation without allocating extra memory (disregarding 
-- the input memory)?
 
import Data.List (sort)

removeNth :: Int -> [a] -> [a]
removeNth 0 (x:xs) = xs
removeNth i (x:xs) = x:(removeNth (pred i) xs)

perms :: [a] -> [[a]]
perms [] = [[]]
perms ls = concat $ fmap (\i -> (fmap (\p -> (ls !! i):p) (perms (removeNth i ls))))  ps
    where
        ps = [0..n]
        n = (length ls) - 1

-- This is very inefficient and obviously consumes alot of memory O(n * n!) 
-- and time O(n!)
nextPermutation :: (Ord a) => [a] -> [a]
nextPermutation ls = 
    if null ords
        then head ps
        else head ords
    where
        ps = sort $ perms ls
        ords = [ p | p <- ps, p > ls ]

test1 :: IO ()
test1 = do
    let ts = [ "123"
             , "132"
             , "321"
             ]
        res = fmap nextPermutation ts
    putStrLn $ show res

main :: IO ()
main = test1
