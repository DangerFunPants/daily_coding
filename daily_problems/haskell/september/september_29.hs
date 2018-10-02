-- This problem was asked by Google.
-- 
-- Given a word W and a string S, find all starting indices in S which are 
-- anagrams of W.
-- 
-- For example, given that W is "ab", and S is "abxaba", return 0, 3, and 4.


removeNth :: Int -> [a] -> [a]
removeNth 0 (x:xs) = xs
removeNth n (x:xs) = x:removeNth (pred n) xs

perms :: [a] -> [[a]]
perms [] = [[]]
perms ls@(x:xs) = concat $ fmap indFn is
    where
        is = [0..(length xs)]
        indFn i = fmap ((ls !! i):) (perms updated)
            where
                updated = removeNth i ls

findAnagrams :: [a] -> [[a]]
findAnagrams = perms

findIndices :: String -> String -> Int -> [Int]
findIndices [] _ _ = []
findIndices ss w i =
    if w `elem` (findAnagrams (take (length w) ss))
        then i : recCall
        else recCall
    where
        recCall = findIndices (tail ss) w (succ i)

test1 :: IO ()
test1 = do
    let w = "ab"
        s = "abxaba"
        res = findIndices s w 0
    putStrLn $ show res

main :: IO ()
main = test1
