-- This problem was asked recently by Google.
-- 
-- Given k sorted singly linked lists, write a function to merge all the lists 
-- into one sorted singly linked list.

import Text.Show.Pretty

mergeLists :: (Ord a) => [[a]] -> [a]
mergeLists (x:xs) = foldl (\v l -> mergeLists' v l) x xs
mergeLists [] = []

mergeLists' :: (Ord a) => [a] -> [a] -> [a]
mergeLists' l1@(x:xs) l2@(v:vs) = 
    if x < v
        then x:(mergeLists' xs l2)
        else v:(mergeLists' l1 vs)
mergeLists' xs [] = xs
mergeLists' [] vs = vs

test1 :: IO ()
test1 = do
    let ts = [ [ 1, 3, 5, 7, 9 ]
             , [ 2, 4, 6, 8, 10 ]
             , [ 13, 14 ]
             , [ -2, 8 ]
             ]
        res = mergeLists ts
    putStrLn $ ppShow res

main :: IO ()
main = test1 


