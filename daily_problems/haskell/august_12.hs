-- This problem was asked by Microsoft.
-- 
-- Given a 2D matrix of characters and a target word, write a function that 
-- returns whether the word can be found in the matrix by going left-to-right, 
-- or up-to-down.
-- 
-- For example, given the following matrix:
-- 
-- [['F', 'A', 'C', 'I'],
--  ['O', 'B', 'Q', 'P'],
--  ['A', 'N', 'O', 'B'],
--  ['M', 'A', 'S', 'S']]
-- 
-- and the target word 'FOAM', you should return true, since it's the leftmost 
-- column. Similarly, given the target word 'MASS', you should return true, 
-- since it's the last row.

wordExists mat word = vert || horz
    where
        horz = word `elem` mat
        vertWords = fmap (\w -> fmap (\str -> str !! w) mat) [0..(pred $ length mat)]
        vert = word `elem` vertWords

testWord :: String -> Bool -> Bool -> String
testWord w e a = w ++ " should be " ++ show e ++ " was " ++ show a

test1 :: IO ()
test1 = do
    let ts = [ "FACI"
             , "OBQP"
             , "ANOB"
             , "MASS"
             ]
        wordList = [ ("FOAM", True), ("MASS", True), ("TEST", False) ]
        res = fmap (\(w,r) -> testWord w r (wordExists ts w)) wordList
    putStrLn $ show res

main :: IO ()
main = test1
