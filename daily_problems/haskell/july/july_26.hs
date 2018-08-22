-- This problem was asked by Amazon.
-- 
-- Given a string, find the longest palindromic contiguous substring. If there 
-- are more than one with the maximum length, return any one.
-- 
-- For example, the longest palindromic substring of "aabcdcb" is "bcdcb". The 
-- longest palindromic substring of "bananas" is "anana".

import Data.List (sortOn)

isPalin :: String -> Bool
isPalin [] = True
isPalin (a:[]) = True
isPalin str = res
    where
        f = head str
        l = last str
        res = if (f == l)
                then isPalin ((init . tail) str)
                else False

mkAllPalins :: String -> [String]
mkAllPalins [] = []
mkAllPalins (a:[]) = []
mkAllPalins str = res++recCall
    where
        branches = [ (tail str), (init str) ]
        res = filter isPalin branches
        recCall = if (null res) 
                    then concat (fmap mkAllPalins branches)
                    else []

longestPalin :: String -> String 
longestPalin = last . (sortOn length) . mkAllPalins

test1 :: IO ()
test1 = do
    let ts = [ "bananas", "aabcdcb" ]
        res = fmap longestPalin ts
    mapM putStrLn res
    return ()

main :: IO ()
main = test1
