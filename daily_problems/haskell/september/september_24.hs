-- This problem was asked by Pinterest.
-- 
-- Given an integer list where each number represents the number of hops you 
-- can make, determine whether you can reach to the last index starting at 
-- index 0.
-- 
-- For example, [2, 0, 1, 0] returns true while [1, 1, 0, 1] returns false.

findRes :: [Int] -> Bool
findRes (x:x':xs) = 
    if x == 0
        then False
        else findRes (drop x (x':xs))
findRes (x:xs) = True

test1 :: IO ()
test1 = do
    let ts = [ [ 2, 0, 1, 0 ]
             , [ 1, 1, 0, 1 ]
             ]
        res = fmap findRes ts
    putStrLn $ show res


main :: IO ()
main = test1
