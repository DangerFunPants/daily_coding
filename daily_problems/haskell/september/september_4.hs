-- This problem was asked by Google.
-- 
-- Given a string of parentheses, write a function to compute the minimum 
-- number of parentheses to be removed to make the string valid (i.e. each open 
-- parenthesis is eventually closed).
-- 
-- For example, given the string "()())()", you should return 1. Given the 
-- string ")(", you should return 2, since we must remove all of them.

countRemoves' :: String -> String -> Int
countRemoves' s ('(':rest) = countRemoves' ('(':s) rest
countRemoves' s (')':rest) = 
    if null s 
        then 1 + countRemoves' s rest
        else countRemoves' (tail s) rest
countRemoves' s [] = length s

countRemoves :: String -> Int
countRemoves = countRemoves' []
test1 :: IO ()
test1 = do
    let ts = [ "()())()"
             , ")("
             ]
        res = fmap countRemoves ts
    putStrLn $ show res

main :: IO ()
main = test1
