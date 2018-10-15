-- This problem was asked by Facebook.
-- 
-- Write a function that rotates a list by k elements. For example, [1, 2, 3, 
-- 4, 5, 6] rotated by two becomes [3, 4, 5, 6, 1, 2]. Try solving this without 
-- creating a copy of the list. How many swap or move operations do you need?


rotateList :: Int -> [a] -> [a]
rotateList 0 ls = ls
-- rotateList k ls = rotateList (pred k) ((last ls):(init ls))
rotateList k ls = rotateList (pred k) ((tail ls) ++ [(head ls)])

test1 :: IO ()
test1 = do
    let ts = [1..6]
        res = rotateList 2 ts
    putStrLn $ show res

main :: IO ()
main = test1
