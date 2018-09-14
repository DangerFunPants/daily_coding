-- This problem was asked by Microsoft.
-- 
-- Given a number in the form of a list of digits, return all possible 
-- permutations.
-- 
-- For example, given [1,2,3], return 
-- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]].

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

test1 :: IO ()
test1 = do
    let ts = [1..3]
        res = perms ts
    putStrLn $ show res

main :: IO ()
main = test1
