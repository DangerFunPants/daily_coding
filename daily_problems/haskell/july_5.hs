#!/usr/bin/env runhaskell

-- This problem was asked by Facebook.
--
-- Implement regular expression matching with the following special characters:
--
--     . (period) which matches any single character
--     * (asterisk) which matches zero or more of the preceding element
--
-- That is, implement a function that takes in a string and a valid regular
-- expression and returns whether or not the string matches the regular
-- expression.
--
-- For example, given the regular expression "ra." and the string "ray", your
-- function should return true. The same regular expression on the string
-- "raymond" should return false.
--
-- Given the regular expression ".*at" and the string "chat", your function
-- should return true. The same regular expression on the string "chats" should
-- return false.

{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Text.ParserCombinators.Parsec
import Text.Parsec.Char

data Regex
    = Plain Char
    | Kleene Regex
    | Wildcard
    | Conj Regex Regex
    | Disjunct Regex Regex
    deriving (Show)

regex :: QuasiQuoter
regex = QuasiQuoter 
    { quoteExp = compileRegex
    , quotePat = notHandled "patterns"
    , quoteType = notHandled "types"
    , quoteDec = notHandles "declarations"
    }
    where notHandled things = error $ 
            things ++ " are not handled by the regex quasiquoter."

compile :: String -> Q Exp
compile s = 
    case (parseRegEx s) of
        Left err -> fail (show err)
        Right regex

regexList = (many1 regexExpr)
regexExpr = choice [(try star), plain, wc]
plain = do
    c <- letter
    return (Plain c)
star = do
    p <- choice [plain, wc]
    (char '*')
    return (Kleene p)
wc = do
    char '.'
    return Wildcard

parseRegEx :: String -> Either ParseError [Regex]
parseRegEx = parse regexList "Failed to parse regex"

evalRegEx :: [Regex] -> String -> Bool
evalRegEx ((Plain c):rest) (s:ss) = 
    if c == s 
        then (evalRegEx rest ss) 
        else False
evalRegEx ((Wildcard):rest) (s:ss) = (evalRegEx rest ss)
-- The reason Regex evaluation is exponential? 
evalRegEx ((Kleene r):rest) (s:ss) = valid
    where
        with = 
            if (evalRegEx [r] [s])
                then (evalRegEx ((Kleene r):rest) ss)
                else False
        without = 
            if (length rest) > 0
                then 
                    if (evalRegEx [(head rest)] [s])
                        then (evalRegEx (tail rest) ss)
                        else False
                else False
        valid = with || without
evalRegEx ((Kleene r):rest) [] = (evalRegEx rest [])
evalRegEx [] [] = True
evalRegEx _ _ = False
                                    
test1 :: IO ()
test1 = do
    let reg = "ra."
        (Right r) = parseRegEx reg
    putStrLn $ "raymond: "++(show (evalRegEx r "raymond"))
    putStrLn $ "ray: "++(show (evalRegEx r "ray"))

test2 :: IO ()
test2 = do
    let reg = ".*at"
        (Right r) = parseRegEx reg
    putStrLn $ "chats: "++(show (evalRegEx r "chats"))
    putStrLn $ "chat: "++(show (evalRegEx r "chat"))

main :: IO ()
main = do
    test1
    test2
