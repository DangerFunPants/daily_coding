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


findWord mat word = horz || vert
    where
        horz = word `elem` mat
        vLen = (pred . length) mat
        vertWords = fmap (\i -> fmap (\j -> (mat !! j) !! i) [0..vLen]) [0..vLen]
        vert = word `elem` vertWords

test1 :: IO ()
test1 = do
    let ts = [ "FACI"
             , "OBQP"
             , "ANOB"
             , "MASS"
             ]
        words = [ "FOAM"
                , "MASS"
                , "NOPE"
                ]
        res = fmap (\w -> (w, (findWord ts w))) words
    putStrLn $ show res

main :: IO ()
main = test1
