-- This problem was asked by Amazon.
-- 
-- Given a string s and an integer k, break up the string into multiple texts 
-- such that each text has a length of k or less. You must break it up so that 
-- words don't break across lines. If there's no way to break the text up, then 
-- return null.
-- 
-- You can assume that there are no spaces at the ends of the string and that 
-- there is exactly one space between each word.
-- 
-- For example, given the string "the quick brown fox jumps over the lazy dog" 
-- and k = 10, you should return: ["the quick", "brown fox", "jumps over", "the 
-- lazy", "dog"]. No string in the list has a length of more than 10.

wrapWords' :: [String] -> String -> Int -> [String]
wrapWords' (w:ws) ln k = if (length newStr) <= k
                            then wrapWords' ws newStr k
                            else ln:(wrapWords' (w:ws) "" k)
    where
        newStr = if null ln then w else ln ++ " " ++ w
wrapWords' [] ln _ = [ln]

wrapWords'' k = (foldl foldFn ([], "")) . words
    where
        foldFn (words, ln) word = if (length newStr) <= k
                                    then (words, newStr)
                                    else ((words ++ [ln]), word)
            where
                newStr = if null ln then word else ln ++ " " ++ word
                      
wrapWords :: String -> Int -> [String]
wrapWords str k = wrapWords' (words str) "" k

test1 :: IO ()
test1 = do
    let ts = "the quick brown fox jumps over the lazy dog"
        res1 = wrapWords ts 10
        res2 = wrapWords'' 10 ts
    putStrLn $ "No explicit fold: " ++ show res1
    putStrLn $ "Explicit fold: " ++ show res2

main :: IO ()
main = do
    test1
