#!/usr/bin/env runhaskell

-- This problem was asked by Amazon.
--
-- Run-length encoding is a fast and simple method of encoding strings. The
-- basic idea is to represent repeated successive characters as a single count
-- and character. For example, the string "AAAABBBCCDAA" would be encoded as
-- "4A3B2C1D2A".
--
-- Implement run-length encoding and decoding. You can assume the string to be
-- encoded have no digits and consists solely of alphabetic characters. You can
-- assume the string to be decoded is valid.

import Data.Char
import Text.ParserCombinators.Parsec

encode :: String -> [(Char, Int)]
encode (a:as) = res:(encode next)
    where
        ls = takeWhile (== a) as
        len = (length ls)
        res = (a, (succ len))
        next = drop len as
encode [] = []

toString :: [(Char, Int)] -> String
toString ((c, n):rest) = (show n)++([c])++(toString rest)
toString [] = ""

fromString :: String -> String
fromString s = 
    case (parse total "Failed to Parse" s) of
        (Right r) -> concat r
        (Left err) -> error (show err)

total = many1 tuple
tuple = do
    n <- ds
    l <- letter
    return (replicate n l)
ds = do
    num <- (many1 digit)
    return (read num)

test1 :: IO () 
test1 = do
    let res = (toString . encode) "AAAABBBCCDAA"
        res' = fromString res
    putStrLn $ show res
    putStrLn $ show res'

main :: IO ()
main = do
    test1