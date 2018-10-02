-- This problem was asked by Google.
-- 
-- Given a string of words delimited by spaces, reverse the words in string. 
-- For example, given "hello world here", return "here world hello"
-- 
-- Follow-up: given a mutable string representation, can you perform this 
-- operation in-place?

intersperse :: [a] -> a -> [a]
intersperse (a:(a':as)) v = (a:[v]) ++ (intersperse (a':as) v)
intersperse as _ = as

answer :: String -> String
answer s = mconcat $ intersperse ((reverse . words) s) " "

test1 :: IO ()
test1 = do
    let ts = "hello world here"
        res = answer ts
    putStrLn $ show res

main :: IO ()
main = test1
