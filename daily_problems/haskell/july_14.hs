#!/usr/bin/env runhaskell

-- This problem was asked by Quora.
-- 
-- Given a string, find the palindrome that can be made by inserting the fewest 
-- number of characters as possible anywhere in the word. If there is more than 
-- one palindrome of minimum length that can be made, return the 
-- lexicographically earliest one (the first one alphabetically).
-- 
-- For example, given the string "race", you should return "ecarace", since we 
-- can add three letters to it (which is the smallest amount to make a 
-- palindrome). There are seven other palindromes that can be made from "race" 
-- by adding three letters, but "ecarace" comes first alphabetically.
-- 
-- As another example, given the string "google", you should return "elgoogle".

-- race
-- erace
-- ecarace

import Data.List (sortOn)

mkPalindrome' :: String -> [String]
mkPalindrome' [] = [[]] -- Got totally fucked over by this...
mkPalindrome' (a:[]) = [[a]]
mkPalindrome' (a:as) = palins
    where
        isPalin = if (a == (last as))
                    then True
                    else False
        leftMost = if isPalin
                    then (init as)
                    else (init (a:as))
        rightMost = if isPalin
                        then (init as)
                        else as
        lastC = [(last as)]
        firstC = [a]
        useLeft = fmap (\st -> lastC++st++lastC) (mkPalindrome' leftMost)
        useRight = fmap (\st -> firstC++st++firstC) (mkPalindrome' rightMost) 
        palins = useLeft++useRight

mkPalindrome :: String -> String
mkPalindrome str = shortest
    where
        possible = mkPalindrome' str
        shortest = head (sortOn length possible)

-- Optimal expansion (google)
--
-- google -> egoogle 
-- googl -> lgoogl
-- goog -> goog
-- oo -> oo
-- "" -> elgoogle
--
-- Optimal Expansion (elgoog)
-- elgoog -> elgooge
-- lgoog -> lgoogl
-- goog -> goog
-- oo -> oo
-- "" -> ""

test1 :: IO ()
test1 = do
    let ts = [ "race"
             , "google"
             , (reverse "google")
             , "egoogl"
             ]
        res = fmap mkPalindrome ts
    mapM (\(o, r) -> putStrLn $ show (o, r)) (zip ts res)
    return ()


main :: IO ()
main = do
    test1
