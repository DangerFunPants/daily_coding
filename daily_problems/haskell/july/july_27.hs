-- This problem was asked by Facebook.
-- 
-- Given a array of numbers representing the stock prices of a company in 
-- chronological order, write a function that calculates the maximum profit you 
-- could have made from buying and selling that stock once. You must buy before 
-- you can sell it.
-- 
-- For example, given [9, 11, 8, 5, 7, 10], you should return 5, since you 
-- could buy the stock at 5 dollars and sell it at 10 dollars.

import Data.List (sortOn)

maxOn :: (Ord b) => (a -> b) -> [a] -> a
maxOn _ [] = error "no elems"
maxOn fn ls = (last . (sortOn fn)) ls

-- Frankly this is terrible. Also O(n^2)
maxVal ls = maxOn (\(a,b) -> (b - a)) [ ((snd a),(snd b)) | a <- (zip [0..] ls), b <- (zip [0..] ls), (fst a) < (fst b) ]

test1 = do
    let ts = [ 9, 11, 8, 5, 7, 10 ]
        res = maxVal ts
    putStrLn $ show res

main :: IO ()
main = test1



