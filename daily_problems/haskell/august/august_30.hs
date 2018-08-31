-- This problem was asked by Yelp.
-- 
-- Given a mapping of digits to letters (as in a phone number), and a digit 
-- string, return all possible letters the number could represent. You can 
-- assume each valid number in the mapping is a single digit.
-- 
-- For example if {“2”: [“a”, “b”, “c”], 3: [“d”, “e”, 
-- “f”], …} then “23” should return [“ad”, “ae”, “af”, 
-- “bd”, “be”, “bf”, “cd”, “ce”, “cf"].

import qualified Data.Map as M

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Don't call fromJust with a Nothing"

possibles :: (Ord a) => [a] -> M.Map a [a] -> [[a]]
possibles [] m = return []
possibles (x:xs) m = possibles xs m >>= \rest -> fmap (\v -> v:rest) is
    where
        is = fromJust $ M.lookup x m

test1 :: IO ()
test1 = do
    let ns = [ ('2', "abc")
             , ('3', "def")
             ]
        m = M.fromList ns
        res = possibles "23" m
    putStrLn $ show res


main :: IO ()
main = test1
