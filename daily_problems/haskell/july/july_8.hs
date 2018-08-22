#!/usr/bin/env runhaskell

-- This problem was asked by Palantir.
--
-- Write an algorithm to justify text. Given a sequence of words and an integer
-- line length k, return a list of strings which represents each line, fully
-- justified.
--
-- More specifically, you should have as many words as possible in each line.
-- There should be at least one space between each word. Pad extra spaces when
-- necessary so that each line has exactly length k. Spaces should be
-- distributed as equally as possible, with the extra spaces, if any,
-- distributed starting from the left.
--
-- If you can only fit one word on a line, then you should pad the right-hand
-- side with spaces.
--
-- Each word is guaranteed not to be longer than k.
--
-- For example, given the list of words ["the", "quick", "brown", "fox",
-- "jumps", "over", "the", "lazy", "dog"] and k = 16, you should return the
-- following:
--
-- ["the  quick brown", # 1 extra space on the left
-- "fox  jumps  over",  # 2 extra spaces distributed evenly
-- "the   lazy   dog"]  # 4 extra spaces distributed evenly

wordList :: [String]
wordList = words "the quick brown fox jumps over the lazy dog"

wrapWords :: [String] -> Int -> [String]
wrapWords wordList k = foldl f [""] wordList
    where
        f (o:os) n = if ((length o) + (length n)) >= k
                        then n:(o:os)
                        else
                            case o of
                                "" -> n:os
                                otherwise -> ((o++" "++n):os)

pad :: Int -> String -> String
pad k s = 
    if (spaces == 0)
        then s++(replicate diff ' ')
        else (loop spMap' s)
    where
        spaces = length (filter (' ' ==) s)
        diff = k - (length s)
        (per, rem) = diff `divMod` spaces
        spMap = replicate spaces (succ per)
        spMap' = fmap f [0..((length spMap) -1)]
        f i = if i < rem
                then (spMap !! i) + 1
                else (spMap !! i)
        loop (m:ms) (' ':as) = (replicate m ' ')++(loop ms as)
        loop ms (a:as) = a:(loop ms as)
        loop [] [] = ""

justifyText :: [String] -> Int -> [String]
justifyText wordList k = reverse $ fmap (pad k) (wrapWords wordList k)

test1 :: IO ()
test1 = do
    let wordList = words "the quick brown fox jumps over the lazy dog"
        k = 16
        w = justifyText wordList k
    putStrLn $ show w
    putStrLn $ show $ fmap length w


main :: IO ()
main = do
    test1